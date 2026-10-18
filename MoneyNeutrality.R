############################################################
## Ordine Cholesky (baseline GDP):
##   gdp, cpi, oil, real wage, wage share, rate
##   => lny, lcpi, l_oil, ln_rw, wage_share, i_*
############################################################


library(vars)
library(svars)
library(urca)
library(tempdisagg)
library(zoo)
library(lubridate)
library(tsibble)
library(dplyr)
library(readxl)
library(readr)
library(janitor)
library(tseries)
library(ggplot2)
library(patchwork)


############################################################
## 1. POLICY: SHORT RATE, EURIBOR, SHADOW, SERIE COMPOSITA
############################################################

## 1.1 – Short rate mensile -> trimestrale (media)
short_rate_m <- read_excel("short term interest rate.xlsx", sheet = "Monthly") %>%
  rename(date = observation_date, short_rate = IR3TIB01ITM156N) %>%
  mutate(date = as.Date(date))

short_rate_q <- short_rate_m %>%
  mutate(quarter = as.yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(short_rate = mean(short_rate, na.rm = TRUE), .groups = "drop") %>%
  arrange(quarter)

## 1.2 – Euribor trimestrale
euribor_raw <- read_csv("euribor.csv", show_col_types = FALSE)

euribor_q <- euribor_raw %>%
  rename(
    date = observation_date,
    euribor3m = IR3TIB01EZQ156N
  ) %>%
  mutate(
    date    = as.Date(date),
    quarter = as.yearqtr(date)
  ) %>%
  select(quarter, euribor3m) %>%
  arrange(quarter)

## 1.3 – Shadow rate mensile -> trimestrale (media)
shadow_raw <- read_excel("shadowrate_ECB.xls", sheet = 1, col_names = FALSE) %>%
  select(1, 2) %>%
  rename(raw_period = 1, shadow = 2) %>%
  mutate(raw_period = as.character(raw_period)) %>%
  filter(!is.na(raw_period), !is.na(shadow)) %>%
  mutate(raw_period = gsub("\\s", "", raw_period))

shadow_clean <- shadow_raw %>%
  mutate(
    year  = as.integer(substr(raw_period, 1, 4)),
    month = as.integer(substr(raw_period, 5, 6)),
    date  = as.Date(sprintf("%04d-%02d-01", year, month))
  ) %>%
  filter(!is.na(date)) %>%
  select(date, shadow) %>%
  arrange(date)

shadow_q <- shadow_clean %>%
  mutate(quarter = as.yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(shadow = mean(shadow, na.rm = TRUE), .groups = "drop") %>%
  arrange(quarter)

## 1.4 – Regimi (per la serie composita)
switch_euro   <- as.yearqtr("1999 Q1")    # Italia -> euro
switch_zlb    <- as.yearqtr("2014 Q1")    # euro -> shadow QE/ZLB (concettuale)
last_shadow_q <- max(shadow_q$quarter, na.rm = TRUE)

## 1.5 – Serie composita i_policy
policy_df <- short_rate_q %>%
  full_join(euribor_q, by = "quarter") %>%
  full_join(shadow_q,  by = "quarter") %>%
  arrange(quarter) %>%
  mutate(
    i_policy = case_when(
      quarter <  switch_euro ~ short_rate,
      quarter >= switch_euro & quarter < switch_zlb ~ euribor3m,
      quarter >= switch_zlb  & quarter <= last_shadow_q ~ shadow,
      quarter >  last_shadow_q ~ euribor3m
    ),
    i_policy = coalesce(i_policy, euribor3m, short_rate)
  ) %>%
  filter(quarter >= as.yearqtr("1980 Q1"),
         quarter <= as.yearqtr("2024 Q4"))

policy_q <- policy_df %>%
  transmute(
    quarter,
    i_policy
  )

## Plot policy composita (opzionale)
plot(as.Date(as.yearqtr(policy_df$quarter)), policy_df$i_policy, type="l",
     xlab="Time", ylab="Policy stance", main="Composed Monetary Policy Rate (1980–2024)")

abline(v = as.Date(as.yearqtr(switch_euro)),   col="red",   lty=2)
abline(v = as.Date(as.yearqtr(switch_zlb)),    col="blue",  lty=2)
abline(v = as.Date(as.yearqtr(last_shadow_q)), col="green", lty=2)
legend("topright",
       legend = c("Euro switch","ZLB/QE start","Return to Euribor"),
       col = c("red","blue","green"), lty = 2)

############################################################
## 2. REAL GDP ITALIA (OECD) – INDICE CHAIN-LINKED
############################################################

realgdp_raw <- read_csv("OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_INDICES,1.1+all.csv",
                        show_col_types = FALSE) %>%
  clean_names()

sub0 <- realgdp_raw %>%
  filter(ref_area == "ITA",
         freq == "Q",
         transaction == "B1GQ",
         unit_measure == "IX")

pb <- if ("LR" %in% unique(sub0$price_base)) "LR" else unique(sub0$price_base)[1]

adj <- if ("adjustment" %in% names(sub0)) {
  u <- unique(sub0$adjustment)
  if ("SCA" %in% u) "SCA" else if ("SA" %in% u) "SA" else u[1]
} else NA_character_

realgdp_it <- sub0 %>%
  filter(price_base == pb,
         if (!is.na(adj) && "adjustment" %in% names(.)) adjustment == adj else TRUE) %>%
  select(time_period, obs_value) %>%
  transmute(
    quarter = as.yearqtr(gsub("-", " ", as.character(time_period))),
    realgdp = as.numeric(obs_value)
  ) %>%
  filter(!is.na(quarter), quarter >= as.yearqtr("1980 Q1")) %>%
  arrange(quarter)

realgdp_q <- realgdp_it %>%
  transmute(
    quarter,
    realgdp
  )

plot(as.Date(realgdp_it$quarter), realgdp_it$realgdp, type="l",
     xlab="Time", ylab="Real GDP (index, chain-linked)")

############################################################
## 3. WAGE RATE, WAGE SHARE, REAL WAGE
############################################################

wage_rate_raw <- read_csv("functional income distribution.csv",
                          show_col_types = FALSE)

## 3.1 – Wage rate (WRT) trimestrale (solo plot)
wage_rate_ita_wrt <- wage_rate_raw %>%
  filter(
    REF_AREA == "ITA",
    FREQ == "Q",
    MEASURE == "WRT",
    grepl("-Q", TIME_PERIOD)
  ) %>%
  mutate(
    quarter   = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
    wage_rate = OBS_VALUE
  ) %>%
  select(quarter, wage_rate) %>%
  arrange(quarter)

wage_rate_ts <- zoo(
  wage_rate_ita_wrt$wage_rate,
  order.by = wage_rate_ita_wrt$quarter
)

plot(wage_rate_ts, main = "Nominal wage rate index – Italy")

## 3.2 – Wage share = Compensation / nominal GDP
comp <- wage_rate_raw %>%
  filter(
    REF_AREA == "ITA",
    MEASURE == "WSSS",
    FREQ == "Q",
    grepl("-Q", TIME_PERIOD)
  ) %>%
  mutate(
    quarter = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
    comp    = OBS_VALUE * 10^UNIT_MULT
  ) %>%
  select(quarter, comp) %>%
  arrange(quarter)

gdp_nom <- wage_rate_raw %>%
  filter(
    REF_AREA == "ITA",
    MEASURE == "GDP",
    FREQ == "Q",
    grepl("-Q", TIME_PERIOD)
  ) %>%
  mutate(
    quarter = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
    gdp_nom = OBS_VALUE * 10^UNIT_MULT
  ) %>%
  select(quarter, gdp_nom) %>%
  arrange(quarter)

wageshare_df <- inner_join(comp, gdp_nom, by = "quarter") %>%
  mutate(wage_share = comp / gdp_nom) %>%
  arrange(quarter)

wageshare_1980_2024_df <- wageshare_df %>%
  filter(
    quarter >= as.yearqtr("1980 Q1"),
    quarter <= as.yearqtr("2024 Q4")
  )

wageshare_q <- wageshare_1980_2024_df %>%
  transmute(
    quarter,
    wage_share
  )

plot(
  x = as.Date(wageshare_1980_2024_df$quarter),
  y = wageshare_1980_2024_df$wage_share,
  type = "l",
  main = "Wage share Italy 1980–2024",
  xlab = "Year",
  ylab = "Wage share"
)

## 3.3 – Real wage = WRT / PGDP (e log real wage)
pgdp <- wage_rate_raw %>%
  filter(
    REF_AREA == "ITA",
    MEASURE == "PGDP",
    FREQ == "Q",
    grepl("-Q", TIME_PERIOD)
  ) %>%
  mutate(
    quarter = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
    pgdp    = OBS_VALUE * 10^UNIT_MULT
  ) %>%
  select(quarter, pgdp) %>%
  arrange(quarter)

wrt <- wage_rate_raw %>%
  filter(
    REF_AREA == "ITA",
    MEASURE == "WRT",
    FREQ == "Q",
    grepl("-Q", TIME_PERIOD)
  ) %>%
  mutate(
    quarter = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
    wrt     = OBS_VALUE * 10^UNIT_MULT
  ) %>%
  select(quarter, wrt) %>%
  arrange(quarter)

realwage_df <- inner_join(wrt, pgdp, by = "quarter") %>%
  mutate(
    real_wage = wrt / pgdp,
    ln_rw     = log(real_wage)
  ) %>%
  arrange(quarter)

realwage_1980_2024_df <- realwage_df %>%
  filter(
    quarter >= as.yearqtr("1980 Q1"),
    quarter <= as.yearqtr("2024 Q4")
  )

realwage_q <- realwage_1980_2024_df %>%
  transmute(
    quarter,
    real_wage,
    ln_rw
  )

plot(
  as.Date(realwage_1980_2024_df$quarter),
  realwage_1980_2024_df$ln_rw,
  type = "l",
  main = "Real wage (product wage, log) Italy 1980–2024",
  xlab = "Year",
  ylab = "log(real wage = WRT / PGDP)"
)

############################################################
## 3.bis – Prezzo del petrolio (Brent) – log livello trimestrale
############################################################

file_excel <- "oil.xlsx"
sheet_name <- "Monthly Prices"

wb_raw <- read_excel(file_excel, sheet = sheet_name, skip = 5)

brent_monthly <- wb_raw %>%
  rename(
    date_str = 1,    # colonna 1 = "1960M01"
    brent    = 3     # colonna 3 = Brent ($/bbl)
  ) %>%
  select(date_str, brent) %>%
  filter(!is.na(date_str), !is.na(brent)) %>%
  mutate(
    date_str = as.character(date_str),
    year  = as.integer(substr(date_str, 1, 4)),
    month = as.integer(substr(date_str, 6, 7)),
    date  = as.Date(sprintf("%04d-%02d-01", year, month))
  ) %>%
  filter(date >= as.Date("1980-01-01"),
         date <= as.Date("2024-12-31")) %>%
  arrange(date) %>%
  select(date, brent)

brent_quarterly_df <- brent_monthly %>%
  mutate(quarter = as.yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(
    brent = mean(brent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter) %>%
  mutate(
    l_oil = log(brent)
  )

oil_q <- brent_quarterly_df %>%
  transmute(
    quarter,
    l_oil
  )

############################################################
## 4. HOUSEHOLD DEBT, UNEMPLOYMENT, CPI (solo grafici)
############################################################

## 4.1 – Household debt-to-GDP (log)
hh_raw <- read_excel(
  "Total Credit to Households and NPISHs, Adjusted for Breaks, for Italy (QITHAM770A).xlsx",
  sheet     = "Quarterly, End of Quarter",
  skip      = 10,
  col_names = c("date", "hh_credit_gdp")
)

hh_debt_df <- hh_raw %>%
  filter(!is.na(date), !is.na(hh_credit_gdp)) %>%
  mutate(
    date    = as.Date(date),
    quarter = as.yearqtr(date)
  ) %>%
  arrange(quarter) %>%
  filter(
    quarter >= as.yearqtr("1980 Q1"),
    quarter <= as.yearqtr("2024 Q4"),
    hh_credit_gdp > 0
  ) %>%
  mutate(ln_hh_credit_gdp = log(hh_credit_gdp))

ggplot(hh_debt_df, aes(x = as.Date(quarter), y = ln_hh_credit_gdp)) +
  geom_line() +
  labs(
    title = "Italy – Household Credit-to-GDP Ratio (log), 1980–2024",
    x = "Year",
    y = "log(credit to households / GDP, %)"
  ) +
  theme_minimal()

## 4.2 – Unemployment trimestrale
unemp <- read.csv("unemployment rate 82-2025 monthly.csv")

unemp_df <- unemp %>%
  mutate(
    date    = as.Date(observation_date),
    quarter = as.yearqtr(date)
  ) %>%
  select(quarter, unemp = LRHUTTTTITM156S)

unemp_q <- unemp_df %>%
  group_by(quarter) %>%
  summarise(unemployment_rate = mean(unemp), .groups = "drop") %>%
  filter(
    quarter >= as.yearqtr("1983 Q1"),
    quarter <= as.yearqtr("2024 Q4")
  )

ggplot(unemp_q, aes(x = as.Date(quarter), y = unemployment_rate)) +
  geom_line() +
  labs(
    title = "Italy – Harmonized Unemployment Rate (Quarterly, 1983–2024)",
    x = "Quarter",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()

## 4.3 – CPI trimestrale, log livello
cpi_raw <- read_csv("ITACPIALLQINMEI.csv")

cpi <- cpi_raw %>%
  rename(
    date = observation_date,
    cpi  = ITACPIALLQINMEI
  ) %>%
  mutate(
    date    = as.Date(date),
    quarter = as.yearqtr(date)
  ) %>%
  arrange(quarter) %>%
  mutate(
    lcpi   = log(cpi),
    pi_qoq = 100 * (lcpi - lag(lcpi)),
    pi_yoy = 100 * (lcpi - lag(lcpi, 4))
  ) %>%
  filter(date >= as.Date("1980-01-01"),
         date <= as.Date("2024-12-31"))

cpi_q <- cpi %>%
  transmute(
    quarter,
    cpi,
    lcpi
  )

ggplot(cpi, aes(x = date, y = cpi)) +
  geom_line() +
  labs(
    title = "Italy CPI (Quarterly)",
    x = "Date",
    y = "CPI (Index, 2015 = 100)"
  ) +
  theme_minimal(base_size = 14)

ggplot(cpi, aes(x = date, y = lcpi)) +
  geom_line() +
  labs(
    title = "Log CPI (Quarterly)",
    x = "Date",
    y = "log(CPI)"
  ) +
  theme_minimal(base_size = 14)

############################################################
## 5. INTERBANK 3-MONTH (MENSILE -> TRIMESTRALE)
############################################################

dati_ib <- read.csv("interbank.csv", stringsAsFactors = FALSE)

tasso_mensile <- ts(
  dati_ib$IR3TIB01ITM156N,
  start     = c(1980, 1),
  frequency = 12
)

tasso_trimestrale <- aggregate(
  tasso_mensile,
  nfrequency = 4,
  FUN        = mean
)

ib_q <- data.frame(
  quarter = as.yearqtr(time(tasso_trimestrale)),
  i_ib3m  = as.numeric(tasso_trimestrale)
) %>%
  filter(
    quarter >= as.yearqtr("1980 Q1"),
    quarter <= as.yearqtr("2024 Q4")
  ) %>%
  arrange(quarter)

plot(tasso_mensile, main = "Tasso interbancario 3 mesi - mensile")
plot(tasso_trimestrale, main = "Tasso interbancario 3 mesi - trimestrale (media trimestrale)")

############################################################
## 6. FUNZIONI DI SUPPORTO VAR/SVAR
############################################################

run_var_diagnostics <- function(var_model, name = "") {
  cat("\n============================\n")
  cat("DIAGNOSTICA VAR:", name, "\n")
  cat("============================\n\n")
  
  cat("1) Serial correlation (Portmanteau test):\n")
  print(serial.test(var_model, lags.pt = 16, type = "PT.asymptotic"))
  
  cat("\n2) ARCH effects:\n")
  print(arch.test(var_model, lags.multi = 4))
  
  cat("\n3) Normalità residui (Jarque–Bera):\n")
  print(normality.test(var_model))
  
  cat("\n4) Radici del VAR (moduli):\n")
  print(Mod(roots(var_model)))
  
  cat("\n5) Stabilità coefficienti (OLS-CUSUM):\n")
  stab <- stability(var_model, type = "OLS-CUSUM")
  plot(stab, main = paste("Stability –", name))
  
  invisible(stab)
}

unitroot_tests <- function(x, name) {
  cat("\n====================================\n")
  cat("Unit root & KPSS per:", name, "\n")
  cat("====================================\n\n")
  
  adf_trend <- ur.df(x, type = "trend", selectlags = "AIC")
  cat("ADF (trend) –", name, "\n")
  print(summary(adf_trend))
  
  adf_drift <- ur.df(x, type = "drift", selectlags = "AIC")
  cat("\nADF (drift) –", name, "\n")
  print(summary(adf_drift))
  
  cat("\nKPSS (level) –", name, "\n")
  print(kpss.test(x, null = "Level"))
  
  cat("\nKPSS (trend) –", name, "\n")
  print(kpss.test(x, null = "Trend"))
  
  invisible(NULL)
}

unitroot_kpss_all <- function(z, name_prefix = "") {
  z <- zoo::zoo(z)
  X <- zoo::coredata(z)
  if (is.null(dim(X))) {
    X <- matrix(X, ncol = 1)
  }
  cn <- colnames(z)
  if (is.null(cn)) {
    cn <- paste0("Var", seq_len(ncol(X)))
  }
  for (j in seq_len(ncol(X))) {
    nm <- cn[j]
    x  <- X[, j]
    x  <- as.numeric(x)
    if (all(is.na(x))) {
      cat("\nSerie", nm, ": tutta NA, salto i test.\n")
      next
    }
    x <- stats::na.omit(x)
    if (length(x) < 10) {
      cat("\nSerie", nm, ": meno di 10 osservazioni utili, salto i test.\n")
      next
    }
    if (length(unique(x)) < 3) {
      cat("\nSerie", nm, ": quasi costante, salto i test ADF/KPSS.\n")
      next
    }
    xt <- ts(x, frequency = 4)
    unitroot_tests(xt, paste0(name_prefix, nm))
  }
  invisible(NULL)
}

############################################################
## 7. COSTRUZIONE DATASET VAR IN LIVELLI CON ALLINEAMENTO 
############################################################

## 7.1 – Aggiungo chiave "YYYY Qn" 
policy_q2 <- policy_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

cpi_q2 <- cpi_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

realgdp_q2 <- realgdp_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

realwage_q2 <- realwage_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

wageshare_q2 <- wageshare_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

ib_q2 <- ib_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

oil_q2 <- oil_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

short_rate_q2 <- short_rate_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

euribor_q2 <- euribor_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

shadow_q2 <- shadow_q %>%
  mutate(qchar = format(quarter, "%Y Q%q"))

## 7.2 – Intersezione trimestri comuni per il VAR con policy composita + oil
common_qchar_policy <- Reduce(
  intersect,
  list(
    policy_q2$qchar,
    cpi_q2$qchar,
    realgdp_q2$qchar,
    realwage_q2$qchar,
    wageshare_q2$qchar,
    oil_q2$qchar
  )
)

common_qchar_policy <- common_qchar_policy[
  common_qchar_policy >= "1983 Q1" &
    common_qchar_policy <= "2024 Q4"
]

## 7.3 – Dataset in livelli per policy composita + oil (se ti serve)
var_df_levels <- data.frame(qchar = common_qchar_policy) %>%
  left_join(realgdp_q2[,   c("qchar","realgdp")],      by = "qchar") %>%
  left_join(cpi_q2[,       c("qchar","lcpi")],         by = "qchar") %>%
  left_join(oil_q2[,       c("qchar","l_oil")],        by = "qchar") %>%
  left_join(realwage_q2[,  c("qchar","ln_rw")],        by = "qchar") %>%
  left_join(wageshare_q2[, c("qchar","wage_share")],   by = "qchar") %>%
  left_join(policy_q2[,    c("qchar","i_policy")],     by = "qchar") %>%
  mutate(
    quarter = as.yearqtr(qchar, format = "%Y Q%q"),
    lny     = log(realgdp)
  ) %>%
  arrange(quarter) %>%
  select(
    quarter,
    lny,         # gdp (log)
    lcpi,        # cpi (log)
    l_oil,       # oil (log Brent)
    ln_rw,       # real wage (log)
    wage_share,  # wage share
    i_policy     # policy rate (composita)
  ) %>%
  filter(complete.cases(.))

############################################################
## 8. SVAR INTERBANK – LIVELLI (VAR 4, livelli + oil)
##    Scelta automatica tra modello con sola costante
##    e modello con costante + trend via BIC
############################################################

common_qchar_ib <- Reduce(
  intersect,
  list(
    ib_q2$qchar,
    cpi_q2$qchar,
    realgdp_q2$qchar,
    realwage_q2$qchar,
    wageshare_q2$qchar,
    oil_q2$qchar
  )
)

## NB: uso 1980Q1–2024Q4
common_qchar_ib <- common_qchar_ib[
  common_qchar_ib >= "1980 Q1" &
    common_qchar_ib <= "2024 Q4"
]

var_df_ib_lev <- data.frame(qchar = common_qchar_ib) %>%
  left_join(realgdp_q2[,   c("qchar","realgdp")],      by = "qchar") %>%
  left_join(cpi_q2[,       c("qchar","lcpi")],         by = "qchar") %>%
  left_join(oil_q2[,       c("qchar","l_oil")],        by = "qchar") %>%
  left_join(realwage_q2[,  c("qchar","ln_rw")],        by = "qchar") %>%
  left_join(wageshare_q2[, c("qchar","wage_share")],   by = "qchar") %>%
  left_join(ib_q2[,        c("qchar","i_ib3m")],       by = "qchar") %>%
  mutate(
    quarter = as.yearqtr(qchar, format = "%Y Q%q"),
    lny     = log(realgdp)
  ) %>%
  arrange(quarter) %>%
  select(
    quarter,
    lny,         # gdp
    lcpi,        # cpi
    l_oil,       # oil (controllo)
    ln_rw,       # real wage
    wage_share,  # wage share
    i_ib3m       # 3m interbank
  ) %>%
  filter(complete.cases(.))

if (nrow(var_df_ib_lev) == 0) {
  stop("Nessuna osservazione comune per il modello interbank in livelli.")
}

diag_zoo_ib_lev <- zoo(
  x        = var_df_ib_lev %>% select(-quarter),
  order.by = var_df_ib_lev$quarter
)

## Test ADF + KPSS (livelli)
unitroot_kpss_all(diag_zoo_ib_lev, "INTERBANK – ")

start_y_ib_lev <- as.integer(format(start(diag_zoo_ib_lev), "%Y"))
start_q_ib_lev <- as.integer(format(start(diag_zoo_ib_lev), "%q"))

var_ts_ib_lev <- ts(
  coredata(diag_zoo_ib_lev),
  start     = c(start_y_ib_lev, start_q_ib_lev),
  frequency = 4
)
colnames(var_ts_ib_lev) <- c("lny","lcpi","l_oil","ln_rw","wage_share","i_ib3m")

## VAR(4): confronto const vs both via AIC/BIC
p_ib <- 4

var_ib_const <- VAR(
  y    = var_ts_ib_lev,
  p    = p_ib,
  type = "const"
)

var_ib_both <- VAR(
  y    = var_ts_ib_lev,
  p    = p_ib,
  type = "both"
)

info_ib <- rbind(
  const = c(AIC = AIC(var_ib_const), BIC = BIC(var_ib_const)),
  both  = c(AIC = AIC(var_ib_both),  BIC = BIC(var_ib_both))
)
cat("\n=== AIC/BIC confronto interbank VAR(4) ===\n")
print(info_ib)

## Scelta del modello sulla base del BIC (più parsimonioso)
if (BIC(var_ib_const) <= BIC(var_ib_both)) {
  var_model_ib_lev <- var_ib_const
  cat("\n=> Selezionato modello interbank VAR(4) con SOLO COSTANTE (type = 'const').\n")
  trend_spec_ib <- "const"
} else {
  var_model_ib_lev <- var_ib_both
  cat("\n=> Selezionato modello interbank VAR(4) con COSTANTE + TREND (type = 'both').\n")
  trend_spec_ib <- "both"
}

summary(var_model_ib_lev)

diag_full_ib_lev <- run_var_diagnostics(
  var_model_ib_lev,
  paste0("Full sample – Interbank rate (type = '", trend_spec_ib, "')")
)

svar_chol_ib_lev <- id.chol(
  x     = var_model_ib_lev,
  order = c("lny","lcpi","l_oil","ln_rw","wage_share","i_ib3m")
)
summary(svar_chol_ib_lev)

set.seed(123)
irf_mp_ib_lev <- irf(
  svar_chol_ib_lev,
  impulse  = "i_ib3m",
  response = c("lny","lcpi","ln_rw","wage_share"),  # niente l_oil nel plot
  n.ahead  = 20,
  boot     = TRUE,
  runs     = 1000,
  ci       = 0.95
)

plot(irf_mp_ib_lev,
     main = "IRF – shock interbank (policy last, VAR(4))")

## Se vuoi il data frame delle IRF medie:
irf_df_ib_lev <- irf_mp_ib_lev$irf

############################################################
## 9. FUNZIONE GENERICA PER REGIMI (POLICY COMPOSITA) – LIVELLI + OIL
##    Cholesky: lny, lcpi, l_oil, ln_rw, wage_share, i_policy
##  
############################################################

estimate_svar_regime_levels <- function(df_sub, p = 2, regime_name = "") {
  # df_sub: quarter, lny, lcpi, l_oil, ln_rw, wage_share, i_policy
  
  var_zoo <- zoo(
    x = df_sub %>% select(-quarter),
    order.by = df_sub$quarter
  )
  
  unitroot_kpss_all(var_zoo, paste0(" - ", regime_name, " – "))
  
  start_y <- as.integer(format(start(var_zoo), "%Y"))
  start_q <- as.integer(format(start(var_zoo), "%q"))
  
  var_ts <- ts(
    coredata(var_zoo),
    start     = c(start_y, start_q),
    frequency = 4
  )
  colnames(var_ts) <- c("lny","lcpi","l_oil","ln_rw","wage_share","i_policy")
  
  var_model <- VAR(
    y    = var_ts,
    p    = p,
    type = "const"   # per i regimi usiamo la specifica con sola costante
  )
  
  cat("\n=== Regime:", regime_name, "===\n")
  print(summary(var_model))
  
  run_var_diagnostics(var_model, paste0(" – ", regime_name))
  
  svar_chol <- id.chol(
    x     = var_model,
    order = c("lny","lcpi","l_oil","ln_rw","wage_share","i_policy")
  )
  
  cat("\n--- SVAR (Cholesky, levels + oil) per regime:", regime_name, "---\n")
  print(summary(svar_chol))
  
  irf_mp <- irf(
    svar_chol,
    impulse  = "i_policy",
    response = c("lny","lcpi","ln_rw","wage_share"),  # niente l_oil nel plot
    n.ahead  = 20,
    boot     = TRUE,
    runs     = 1000,
    ci       = 0.95
  )
  
  list(
    data      = df_sub,
    var_ts    = var_ts,
    var_model = var_model,
    svar      = svar_chol,
    irf_mp    = irf_mp
  )
}

############################################################
## 10. REGIME 1 – ITALIAN SHORT RATE 1980–1999 – LIVELLI + OIL, VAR(2)
############################################################

common_qchar_short <- Reduce(
  intersect,
  list(
    short_rate_q2$qchar,
    cpi_q2$qchar,
    realgdp_q2$qchar,
    realwage_q2$qchar,
    wageshare_q2$qchar,
    oil_q2$qchar
  )
)

common_qchar_short <- common_qchar_short[
  common_qchar_short >= "1980 Q1" &
    common_qchar_short <= "1999 Q4"
]

var_df_short_italy <- data.frame(qchar = common_qchar_short) %>%
  left_join(realgdp_q2[,    c("qchar","realgdp")],    by = "qchar") %>%
  left_join(cpi_q2[,        c("qchar","lcpi")],       by = "qchar") %>%
  left_join(oil_q2[,        c("qchar","l_oil")],      by = "qchar") %>%
  left_join(realwage_q2[,   c("qchar","ln_rw")],      by = "qchar") %>%
  left_join(wageshare_q2[,  c("qchar","wage_share")], by = "qchar") %>%
  left_join(short_rate_q2[, c("qchar","short_rate")], by = "qchar") %>%
  mutate(
    quarter  = as.yearqtr(qchar, format = "%Y Q%q"),
    lny      = log(realgdp),
    i_policy = short_rate
  ) %>%
  arrange(quarter) %>%
  select(
    quarter,
    lny,
    lcpi,
    l_oil,
    ln_rw,
    wage_share,
    i_policy
  ) %>%
  filter(complete.cases(.))

if (nrow(var_df_short_italy) == 0) {
  stop("Nessuna osservazione comune per short rate Italia 1980–1999.")
}

res_italy_lev <- estimate_svar_regime_levels(
  df_sub      = var_df_short_italy,
  p           = 2,
  regime_name = "Italy – short rate 1980–1999"
)

plot(res_italy_lev$irf_mp,
     main = "IRF – shock Italian short rate (1980–1999, VAR(2))")

############################################################
## 11. REGIME 2 – EURIBOR 1999–2015 – LIVELLI + OIL, VAR(2)
############################################################

common_qchar_euribor <- Reduce(
  intersect,
  list(
    euribor_q2$qchar,
    cpi_q2$qchar,
    realgdp_q2$qchar,
    realwage_q2$qchar,
    wageshare_q2$qchar,
    oil_q2$qchar
  )
)

common_qchar_euribor <- common_qchar_euribor[
  common_qchar_euribor >= "1999 Q1" &
    common_qchar_euribor <= "2015 Q4"
]

var_df_euribor <- data.frame(qchar = common_qchar_euribor) %>%
  left_join(realgdp_q2[,   c("qchar","realgdp")],    by = "qchar") %>%
  left_join(cpi_q2[,       c("qchar","lcpi")],       by = "qchar") %>%
  left_join(oil_q2[,       c("qchar","l_oil")],      by = "qchar") %>%
  left_join(realwage_q2[,  c("qchar","ln_rw")],      by = "qchar") %>%
  left_join(wageshare_q2[, c("qchar","wage_share")], by = "qchar") %>%
  left_join(euribor_q2[,   c("qchar","euribor3m")],  by = "qchar") %>%
  mutate(
    quarter  = as.yearqtr(qchar, format = "%Y Q%q"),
    lny      = log(realgdp),
    i_policy = euribor3m
  ) %>%
  arrange(quarter) %>%
  select(
    quarter,
    lny,
    lcpi,
    l_oil,
    ln_rw,
    wage_share,
    i_policy
  ) %>%
  filter(complete.cases(.))

if (nrow(var_df_euribor) == 0) {
  stop("Nessuna osservazione comune per Euribor 1999–2015.")
}

res_euribor_lev <- estimate_svar_regime_levels(
  df_sub      = var_df_euribor,
  p           = 2,
  regime_name = "Euribor 1999–2015"
)

plot(res_euribor_lev$irf_mp,
     main = "IRF – shock Euribor (1999–2015, VAR(2))")

############################################################
## 12. IRF “GRANDI” – CONFRONTO SHOCK MONETARI (LIVELLI + OIL)
##     SOLO VARIABILI DOMESTICHE: lny, lcpi, ln_rw, wage_share
############################################################

## 12.1 – Interbank
irf_df_ib_lev <- irf_mp_ib_lev$irf

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(irf_df_ib_lev[["epsilon[ i_ib3m ] %->% lny"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock interbank → log GDP")
abline(h = 0, col = "red")

plot(irf_df_ib_lev[["epsilon[ i_ib3m ] %->% lcpi"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock interbank → log CPI")
abline(h = 0, col = "red")

plot(irf_df_ib_lev[["epsilon[ i_ib3m ] %->% ln_rw"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock interbank → log real wage")
abline(h = 0, col = "red")

plot(irf_df_ib_lev[["epsilon[ i_ib3m ] %->% wage_share"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock interbank → wage share")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

## 12.2 – Italian short rate
irf_df_ita_lev <- res_italy_lev$irf_mp$irf

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(irf_df_ita_lev[["epsilon[ i_policy ] %->% lny"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Italian rate → log GDP")
abline(h = 0, col = "red")

plot(irf_df_ita_lev[["epsilon[ i_policy ] %->% lcpi"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Italian rate → log CPI")
abline(h = 0, col = "red")

plot(irf_df_ita_lev[["epsilon[ i_policy ] %->% ln_rw"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Italian rate → log real wage")
abline(h = 0, col = "red")

plot(irf_df_ita_lev[["epsilon[ i_policy ] %->% wage_share"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Italian rate → wage share")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

## 12.3 – Euribor
irf_df_euribor_lev <- res_euribor_lev$irf_mp$irf

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(irf_df_euribor_lev[["epsilon[ i_policy ] %->% lny"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Euribor → log GDP")
abline(h = 0, col = "red")

plot(irf_df_euribor_lev[["epsilon[ i_policy ] %->% lcpi"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Euribor → log CPI")
abline(h = 0, col = "red")

plot(irf_df_euribor_lev[["epsilon[ i_policy ] %->% ln_rw"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Euribor → log real wage")
abline(h = 0, col = "red")

plot(irf_df_euribor_lev[["epsilon[ i_policy ] %->% wage_share"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock Euribor → wage share")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

############################################################
## 13. SHADOW 2015–2022 – LIVELLI + OIL, VAR(2)
############################################################

## shadow_q2 è già costruito sopra

## Intersezione trimestri comuni, dal 2015Q1 al 2022Q4
common_qchar_shadow <- Reduce(
  intersect,
  list(
    shadow_q2$qchar,
    cpi_q2$qchar,
    realgdp_q2$qchar,
    realwage_q2$qchar,
    wageshare_q2$qchar,
    oil_q2$qchar
  )
)

common_qchar_shadow <- common_qchar_shadow[
  common_qchar_shadow >= "2015 Q1" &
    common_qchar_shadow <= "2022 Q4"
]

var_df_shadow_2015_22 <- data.frame(qchar = common_qchar_shadow) %>%
  left_join(realgdp_q2[,   c("qchar","realgdp")],      by = "qchar") %>%
  left_join(cpi_q2[,       c("qchar","lcpi")],         by = "qchar") %>%
  left_join(oil_q2[,       c("qchar","l_oil")],        by = "qchar") %>%
  left_join(realwage_q2[,  c("qchar","ln_rw")],        by = "qchar") %>%
  left_join(wageshare_q2[, c("qchar","wage_share")],   by = "qchar") %>%
  left_join(shadow_q2[,    c("qchar","shadow")],       by = "qchar") %>%
  mutate(
    quarter  = as.yearqtr(qchar, format = "%Y Q%q"),
    lny      = log(realgdp),
    i_shadow = shadow
  ) %>%
  arrange(quarter) %>%
  select(
    quarter,
    lny,
    lcpi,
    l_oil,
    ln_rw,
    wage_share,
    i_shadow
  ) %>%
  filter(complete.cases(.))

if (nrow(var_df_shadow_2015_22) == 0) {
  stop("Nessuna osservazione comune per il modello shadow 2015–2022.")
}

shadow_zoo <- zoo(
  x        = var_df_shadow_2015_22 %>% select(-quarter),
  order.by = var_df_shadow_2015_22$quarter
)

start_y_sh <- as.integer(format(start(shadow_zoo), "%Y"))
start_q_sh <- as.integer(format(start(shadow_zoo), "%q"))

shadow_ts <- ts(
  coredata(shadow_zoo),
  start     = c(start_y_sh, start_q_sh),
  frequency = 4
)
colnames(shadow_ts) <- c("lny","lcpi","l_oil","ln_rw","wage_share","i_shadow")

p_shadow <- 2

var_shadow_2015_22 <- VAR(
  y    = shadow_ts,
  p    = p_shadow,
  type = "const"
)
summary(var_shadow_2015_22)

svar_shadow_2015_22 <- id.chol(
  x     = var_shadow_2015_22,
  order = c("lny","lcpi","l_oil","ln_rw","wage_share","i_shadow")
)
summary(svar_shadow_2015_22)

set.seed(123)
irf_shadow_2015_22 <- irf(
  svar_shadow_2015_22,
  impulse  = "i_shadow",
  response = c("lny","lcpi","ln_rw","wage_share"),  # niente l_oil nel plot
  n.ahead  = 20,
  boot     = TRUE,
  runs     = 1000,
  ci       = 0.95
)

plot(
  irf_shadow_2015_22,
  main = "IRF – shock shadow rate (2015–2022, VAR(2), livelli + oil, policy last)"
)

## IRF “grandi” shadow 2015–2022 (2x2, senza oil)
irf_mean_sh <- irf_shadow_2015_22$irf

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(irf_mean_sh[["epsilon[ i_shadow ] %->% lny"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock shadow → log GDP")
abline(h = 0, col = "red")

plot(irf_mean_sh[["epsilon[ i_shadow ] %->% lcpi"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock shadow → log CPI")
abline(h = 0, col = "red")

plot(irf_mean_sh[["epsilon[ i_shadow ] %->% ln_rw"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock shadow → log real wage")
abline(h = 0, col = "red")

plot(irf_mean_sh[["epsilon[ i_shadow ] %->% wage_share"]],
     type = "l", xlab = "Quarter", ylab = "Response",
     main = "Shock shadow → wage share")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

############################################################
## 14. IRF 2x2 CON BOOTSTRAP (svars::mb.boot)
## Shock di politica -> lny, lcpi, ln_rw, wage_share
##
## Modelli:
##  - svar_chol_ib_lev              (interbank 3m, i_ib3m)
##  - res_italy_lev$svar            (Italia 1980–1999, i_policy = short rate)
##  - res_euribor_lev$svar          (Euribor 1999–2015, i_policy = euribor)
##  - svar_shadow_2015_22           (shadow 2015–2022, i_shadow)
##
## Ordine variabili nei VAR/SVAR:
##   1 = lny
##   2 = lcpi
##   3 = l_oil
##   4 = ln_rw
##   5 = wage_share
##   6 = tasso di interesse
############################################################

library(svars)

## Funzione di comodo:
##  - fa mb.boot
##  - plottando SHOCK = 6 (tasso) -> RISPOSTE = 1 (lny), 2 (lcpi), 4 (ln_rw), 5 (wage_share)
plot_svar_boot_2x2 <- function(svar_obj, main_title = "") {
  set.seed(123)
  
  bb <- mb.boot(
    x        = svar_obj,
    design   = "fixed",
    b.length = 1,
    nboot    = 1000,
    n.ahead  = 20,
    nc       = 1,
    signrest = "null"
  )
  
  resp_idx  <- c(1, 2, 4, 5)
  shock_idx <- 6
  
  plot(
    bb,
    lowerq     = 0.05,
    upperq     = 0.95,
    percentile = "hall",
    selection  = list(resp_idx, shock_idx),
    main       = main_title
  )
}

############################################################
## 14.1) SVAR INTERBANK 3M – shock i_ib

############################################################
## TABELLA IRF – SVAR INTERBANK (i_ib3m, VAR(4))
##  - First period
##  - Last period (20)
##  - Highest value (with period)
##  - Lowest value (with period)
############################################################

## IRF per tutte le variabili in risposta a uno shock di i_ib3m
irf_ib_full <- irf(
  svar_chol_ib_lev,
  impulse  = "i_ib3m",
  response = c("lny","lcpi","l_oil","ln_rw","wage_share","i_ib3m"),
  n.ahead  = 20,
  boot     = FALSE
)

## Lista con i vettori IRF per ciascuna variabile
irf_list <- irf_ib_full$irf
names(irf_list)
# Dovresti vedere qualcosa tipo:
# "lny" "lcpi" "l_oil" "ln_rw" "wage_share" "i_ib3m"

## Funzione di comodo per estrarre gli indicatori per una singola variabile
summarise_irf <- function(var_name, irf_vec) {
  h <- seq_along(irf_vec)      # orizzonti 1,...,20
  
  data.frame(
    variable      = var_name,
    first_h       = 1,
    first_value   = irf_vec[1],
    last_h        = max(h),
    last_value    = irf_vec[length(irf_vec)],
    max_value     = max(irf_vec, na.rm = TRUE),
    max_h         = h[which.max(irf_vec)],
    min_value     = min(irf_vec, na.rm = TRUE),
    min_h         = h[which.min(irf_vec)]
  )
}

## Applico la funzione a tutte le variabili e rbind
irf_summary_ib <- do.call(
  rbind,
  Map(summarise_irf, names(irf_list), irf_list)
)

## Stampa la tabella


print(irf_summary_ib)

library(dplyr)
library(stringr)



## tieni solo le righe con shock i_ib3m
irf_summary_ib_mp <- irf_summary_ib %>%
  filter(str_detect(variable, "i_ib3m"))

print(irf_summary_ib_mp)


