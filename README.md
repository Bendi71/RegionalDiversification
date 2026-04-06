# EFFECTIVENESS OF REGIONAL DIVERSIFICATION IN EUROPEAN STOCK MARKETS

## Research Motivation
European equity markets are often treated as a broad, integrated region, but practical portfolio construction still depends on whether regional segmentation offers real diversification and hedging benefits.

This study examines whether regional diversification inside Europe remains effective under time-varying volatility and correlation dynamics, especially during stress periods. It also evaluates whether adding commodity assets (gold and oil) improves risk reduction in European stock portfolios.

## Abstract
In this paper, I analyzed the effectiveness of regional diversification and hedging strategies using an asymmetric dynamic conditional correlation GARCH (ADCC-GARCH) model. I also conducted statistical tests on hedging effectiveness. The analysis was performed on daily one-step-ahead forecasts derived from the ADCC-GARCH model for the period between 2021 and 2025, covering 12 European countries as well as an index representing the global market.

I grouped the stock indices using clustering, among which the dynamic conditional correlation indicates the phenomenon of financial contagion, and the correlations increased during crisis periods such as the outbreak of the Russian-Ukrainian war or the US tariff announcements.

Among the hedging strategies, the Hungarian-Polish market provides the cheapest hedge, while the Austrian-Czech market is more expensive but more effective. Hedging Western European markets with the global market is less effective than with the Hungarian-Polish markets. From the perspective of diversification, the Hungarian-Polish market is not a significantly effective diversification tool for the other European regions, whereas the Austrian-Czech markets are, while also resulting in better returns. Hedging with the British-Swiss-Swedish markets yielded higher returns than diversification, as this region underperformed the other examined groups.

The global market provides balanced diversification effectiveness across the entire European stock market. Comparing the risk reduction effectiveness of commodity market assets with stock markets, neither gold nor oil significantly reduces variance in the hedging portfolios. During diversification, the safe-haven role of gold becomes apparent; however, oil is not an effective diversification tool for European stock markets. The paper highlights the limitations of regional diversification in European stock markets.

## Data and Scope
- Frequency: daily returns
- Forecast horizon: one-step-ahead
- Core analysis period for forecasts: 2021-2025
- Markets: 12 European country indices + global market proxy
- Additional assets: gold and oil (commodity extension)
- Currency handling: non-EUR indices are converted to EUR before return calculation

## Methodology Summary
1. Data preparation and EUR conversion of index levels.
2. Log-return construction and descriptive statistics.
3. Pre-model diagnostics:
   - ADF, KPSS, Jarque-Bera tests
   - ARCH effect tests
4. Regional grouping by hierarchical DTW clustering.
5. Multivariate volatility modeling:
   - Candidate DCC/ADCC specifications
   - Rolling one-step covariance/correlation forecasts
   - Model selection with Model Confidence Set (MCS)
6. Dynamic hedging evaluation:
   - Time-varying hedge ratios
   - Hedging effectiveness based on variance reduction
   - Statistical testing (F-test, Brown-Forsythe style approach)
7. Commodity augmentation (gold/oil) and VaR-oriented diagnostics.

## Main Findings
- Dynamic correlations rose in crisis episodes, consistent with financial contagion.
- Hungarian-Polish pairings provided lower-cost hedging opportunities.
- Austrian-Czech pairings delivered stronger (but costlier) hedging effectiveness.
- Hedging Western Europe with the global index was less effective than with Hungarian-Polish markets.
- The Hungarian-Polish block was not consistently significant as a diversification tool for other regions.
- Austrian-Czech diversification produced better return-risk outcomes.
- British-Swiss-Swedish hedging generated higher returns than diversification in this sample, linked to weaker regional performance.
- The global market acted as a balanced, broad diversifier across Europe.
- Gold and oil did not materially reduce hedging variance in stock-commodity hedge portfolios.
- Gold showed safe-haven characteristics in diversification settings; oil did not.

## Repository Contents
- Main analysis script: `adatelemzes.R`
- Input CSV files: market and commodity price series
- Output folders:
  - `dcc_plots/`
  - `group_corr_plots/`
  - `hedge_plots/`
  - `hedge_plots_grouped/`
  - `weight_plots_grouped/`
- Exported summary files (examples):
  - `descr_stats.csv`
  - `descr_stats_pct.csv`
  - `coef_roll.csv`
  - `coef_roll_commod.csv`

## Reproducibility Notes
- The script relies on Yahoo Finance downloads for the dataset (`quantmod::getSymbols`).
- Results can vary slightly due to data revisions, market holiday alignment, and rolling re-estimation windows.
- Required R packages are loaded at the top of `adatelemzes.R`.

## Author Note
This repository documents an empirical investigation into the limits of regional diversification in Europe under time-varying dependence. The practical implication is that geographic diversification alone may not provide stable protection during systemic stress, so dynamic hedging and market-regime awareness remain essential.
