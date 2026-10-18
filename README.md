# Is Money Neutral? Monetary Policy and Functional Income Distribution

**A SVAR Analysis on Italy, 1980–2022**

Silvia Pagliarulo — A.Y. 2024–2025, Sant'Anna School of Advanced Studies
Tutor: Prof. Alessio Moneta · Relators: Prof. Giuseppe Fontana, Dott. Antonino Lofaro

## Overview

The study estimates the impact of monetary policy on Italy's functional income
distribution across three monetary regimes:

1. **Pre-euro national regime** (1980–1998): Bank of Italy, interbank/national rate;
2. **Euro regime** (1999–2014): ECB, Euribor 3M;
3. **Unconventional MP** (2015–2022): ZLB, QE, shadow rate.

Conclusion: **money is not neutral** — monetary policy has long-term,
persistent distributional effects.

## Key Findings

| Regime | GDP | Prices | Real Wage | Wage Share |
|--------|-----|--------|-----------|------------|
| Full sample (interbank) | ↓ recessionary | ↓ disinflationary | ↓ short-run, ↑ medium-run | ↑ medium-run, ↓ long-run |
| Pre-euro (1980–98) | ↓ | ↑ then ↓ (price puzzle) | ↓ persistent | ↑ transitory |
| Euro (1999–2014) | ↑ then ↓ | ↓ persistent | ↑ then restrained | ↑ medium-run, → long-run |
| UMP (2015–22) | ↑ expansionary | ↑ persistent | ↑ gradual | ↓ persistent |

## Theoretical Framework

Contrasts New Consensus Macroeconomics (money neutral in the long run) with
endogenous money theory (interest rate as a distributional variable).
Transmission channels: markup cost, cost of capital, production-function
features, labour-market frictions.

## Methodology

**SVAR with Cholesky decomposition.** Ordering: GDP → CPI → Oil → Wage Share
→ Real Wage → Interest Rate. VAR(4) for the full sample, VAR(2) per regime;
estimated in levels with constant and linear trend; interest rate placed last.

**Robustness.** (1) Unemployment substituted for GDP; (2) deterministic-trend
specification; (3) alternative Cholesky orderings.

## Data

| Variable | Source | Period |
|----------|--------|--------|
| Interbank rate (3M) | FRED | 1980–2022 |
| Italian T-Bill rate | OECD/FRED | 1980–1999 |
| Euribor 3M | OECD/FRED | 1999–2015 |
| Shadow rate (Wu-Xia) | Wu & Xia (2016) | 2004–2022 |
| Real GDP / Wage Share / Real Wage / CPI | OECD | 1980–2022 |
| Oil price (Brent) | World Bank | 1980–2022 |
| Unemployment | OECD/FRED | 1980–2022 |



## Key References

- Lofaro & Di Bucchianico (2025). Impact of monetary policy on functional income distribution. Economic Modelling, 151.
- Cantore, Ferroni & León-Ledesma (2021). The missing link. JEEA, 19(3).
- Fontana (2006). The "new consensus" view of monetary policy. EJEEP, 3(2).
- Sims (1980). Macroeconomics and reality. Econometrica, 48(1).
- Wu & Xia (2016). Measuring the macroeconomic impact of MP at the ZLB. JMCB, 48(2-3).
