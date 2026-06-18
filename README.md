# Final Degree Project (TFG) - Extreme Value Theory Applied to Systemic Risk in Cryptocurrency Markets

This repository contains the code, data workflows, and documentation for my Final Degree Project (TFG) in Statistics at Universitat Autònoma de Barcelona.

## Project Overview

The main goal of this project is to analyse the transmission of extreme risk between Bitcoin (BTC) and the main alternative cryptocurrencies (ETH, BNB, XRP, SOL) using Extreme Value Theory (EVT). The work combines high-frequency data with tail risk modelling to quantify systemic contagion in cryptocurrency markets.

Two complementary objectives structure the analysis: characterising the tail behaviour of loss distributions through POT models and GPD fits, and quantifying extreme dependence between assets via the Conditional Value-at-Risk (CoVaR). The analysis focuses on identifying contagion mechanisms during episodes of extreme stress and evaluating the extent to which systemic risk propagates from Bitcoin to the rest of the market.

## Key Components

- **Data Collection**  
  Automated retrieval of high-frequency cryptocurrency price data via REST APIs using Python (`requests`, `pandas`, `numpy`). The script `criptoapi.py` handles API queries, rate limiting, and data structuring.

- **Descriptive Analysis** (`Analisi Descriptiva.R`)  
  Summary statistics, Jarque-Bera normality tests, autocorrelation (ACF) of log-returns and absolute returns, and quantile analysis across 1-minute, 1-hour, and 1-day frequencies.

- **Correlation Analysis** (`Analisi de correlacions.R`)  
  Spearman correlation matrices across temporal frequencies to characterise linear dependence structure between assets.

- **Bivariate Structure & Quadrant Decomposition** (`Grafics.R`)  
  Decomposition of the joint return space into four sign-based quadrants with bivariate normal mixture models fitted per regime, capturing asymmetric dependence between asset pairs.

- **Univariate EVT Analysis** (`Analisi Univariant.R`)  
  Tail diagnostics (Mean Excess Plot, Hill plot, Mean Residual Life Plot, cvplot), threshold selection via `thrselect()` from the `ercv` package, and GPD fitting using `fitpot()`. Extreme VaR estimation at confidence levels up to 99.999%.

- **Normal Mixture Modelling** (`Mixtura de normals.R`)  
  Gaussian mixture models applied to bivariate return distributions to account for regime-dependent dependence structure.

- **Graphics & Visualisation** (`Histogrames.R`)  
  Diagnostic plots, tail behaviour visualisations, and distributional graphics using `ggplot2`, `gridExtra`, `ggfortify`, and `scales`.

- **CoVaR & Systemic Risk** (`Analisi Covariant.R`)  
  Estimation of the Conditional Value-at-Risk (CoVaR) and ∆CoVaR conditioning on extreme quantiles of Bitcoin losses (q ∈ {0.990, …, 0.995}), across CoVaR confidence levels from 99.9% to 99.999%.

## Repository Structure

```
├── criptoapi.py              # Python script for high-frequency data retrieval
├── Analisi Descriptiva.R     # Descriptive statistics and normality tests
├── Analisi de correlacions.R # Correlation analysis
├── Analisi Univariant.R      # Univariate EVT: threshold selection and GPD fitting
├── Analisi Covariant.R       # CoVaR and ΔCoVaR estimation
├── Mixtura de normals.R      # Bivariate normal mixture models
├── Grafics.R                 # Simulated example plots
├── Histogrames.R             # Histogram visualisations
└── TFG-Teoria-de-Valors-Extrems.Rproj  # RStudio project file
```

## Main Results

- All five cryptocurrencies exhibit heavy-tailed loss distributions, high kurtosis, and strong departures from normality, confirmed by Jarque-Bera tests and QQ-plots at all temporal frequencies.
- Positive Extreme Value Index (EVI) estimates across all assets confirm the presence of Pareto-type heavy tails, with implied non-existence of higher-order moments in several cases.
- CoVaR analysis reveals a clear contagion effect: as Bitcoin stress levels increase, the conditional risk of all altcoins rises systematically.
- SOL and BNB emerge as the most vulnerable assets, with the highest CoVaR and ∆CoVaR values across all scenarios. ETH shows intermediate exposure, while XRP displays relatively greater resilience to BTC-originated shocks.
- Extreme dependence between assets is substantially more intense than suggested by traditional Spearman correlations, highlighting the importance of EVT-based conditional risk measures for stress-period analysis.

## Dependencies

**R packages:** `data.table`, `dplyr`, `tidyr`, `zoo`, `ggplot2`, `gridExtra`, `ggfortify`, `scales`, `moments`, `tseries`, `FinTS`, `xtable`, `evir`, `ismev`, `POT`, `ercv`, `fitdistrplus`, `VGAM`, `MASS`, `mvtnorm`

**Python packages:** `requests`, `pandas`, `numpy`, `datetime`, `time`, `argparse`, `sys`, `os`, `typing`

## Authors

- Author: Jordi Anguera Costa
- Tutor: Isabel Serra Mochales

Universitat Autònoma de Barcelona
