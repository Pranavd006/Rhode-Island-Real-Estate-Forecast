# Rhode Island Real Estate Price Forecasting

## Project Overview
This project analyzes and forecasts monthly median listing prices in Rhode Island using time series methods in R. The goal is to identify recent pricing patterns, compare multiple forecasting approaches, and produce an interpretable short-term forecast that can support market monitoring and business-oriented decision-making.

The project uses the Federal Reserve Economic Data (FRED) series `MEDLISPRIRI`, which tracks **Housing Inventory: Median Listing Price in Rhode Island** in U.S. dollars on a monthly, not seasonally adjusted basis. The source file provided for this project includes observations from **July 2016 through March 2026**.

## Business Objective
The objective of this analysis is to forecast near-term Rhode Island housing listing prices and evaluate which time series models best capture the trend and seasonality in the data. From an analytics perspective, this project demonstrates how forecasting can be applied to real-estate market monitoring, pricing trend analysis, and scenario-based planning.

## Dataset
- **Source:** Federal Reserve Economic Data (FRED), Federal Reserve Bank of St. Louis
- **Series ID:** `MEDLISPRIRI`
- **Series Name:** Housing Inventory: Median Listing Price in Rhode Island
- **Frequency:** Monthly
- **Units:** U.S. Dollars
- **Adjustment:** Not Seasonally Adjusted
- **Observed history in provided file:** July 2016 to March 2026
- **Source file included in repository:** `MEDLISPRIRI.xlsx`

## Tools and Skills Demonstrated
- R for time series analysis and forecasting
- Data preparation and transformation
- Exploratory analysis of housing market price movements
- Model comparison across multiple forecasting approaches
- Forecast uncertainty communication using prediction intervals
- Business-focused presentation of analytical findings

Recommended R packages for reproducibility:
- `tidyverse`
- `lubridate`
- `tsibble`
- `fable`
- `feasts`
- `fpp3`
- `readxl`

## Project Workflow
1. Import monthly Rhode Island median listing price data from Excel.
2. Clean and prepare the data for time series analysis.
3. Explore trend, seasonal movement, and recent market changes.
4. Fit and compare multiple forecasting models.
5. Select the strongest candidate models based on performance and interpretability.
6. Generate future forecasts with 80% and 95% prediction intervals.
7. Present results through tables and a forecast visualization.

## Forecasting Approach
The project compares multiple time series models and highlights three leading models in the final visualization:
- `ETS_MAM`
- `ARIMA_man`
- `TSLM_seas`

These models were carried forward as the top model set in the final comparison chart, which visualizes the historical series together with future forecasts and uncertainty bands.

## Key Findings
- The Rhode Island median listing price series shows a clear long-term upward movement over the observed period, rising from the low-to-mid $300,000 range in 2016 to above $500,000 in recent years.
- The historical data also shows recurring seasonal movement, which supports the use of seasonal forecasting models.
- The final forecast output extends from **June 2026 through May 2027** and includes point forecasts along with 80% and 95% prediction intervals.
- The attached project outputs suggest continued price strength in the near term, with expected monthly values generally remaining in the mid-$500,000 to low-$600,000 range across the forecast horizon.

## Forecast Output Snapshot
Selected values from the final forecast table:

| Month | Point Forecast | 80% Interval | 95% Interval |
|---|---:|---:|---:|
| 2026 Jun | $606,065 | $586,156 to $625,975 | $575,616 to $636,514 |
| 2026 Dec | $553,365 | $505,097 to $601,633 | $479,546 to $627,184 |
| 2027 May | $605,327 | $535,454 to $675,200 | $498,465 to $712,188 |

## Repository Structure
```text
rhode-island-real-estate-forecast/
├── README.md
├── .gitignore
├── data/
│   ├── raw/
│   │   └── MEDLISPRIRI.xlsx
│   └── processed/
│       └── final_forecast_table.csv
├── images/
│   └── 05b_best3_forecast.jpg
├── scripts/
│   ├── 01_data_import.R
│   ├── 02_data_cleaning_eda.R
│   ├── 03_model_comparison.R
│   └── 04_final_forecast.R
└── outputs/
    └── forecast_summary.csv
```

## Suggested Script Responsibilities
- `01_data_import.R`: Load the Excel file, rename variables, convert dates, and create a tidy time series object.
- `02_data_cleaning_eda.R`: Check missing values, inspect patterns, visualize trend and seasonality, and summarize the series.
- `03_model_comparison.R`: Fit candidate models and compare forecast accuracy metrics.
- `04_final_forecast.R`: Refit the selected model or shortlisted models, generate forecasts, and export final outputs.

## How to Reproduce
1. Clone the repository.
2. Place the source data file inside `data/raw/`.
3. Install the required R packages.
4. Run the scripts in numerical order.
5. Review exported forecast tables and visual outputs in the project folders.

Example package installation in R:

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "tsibble",
  "fable",
  "feasts",
  "fpp3",
  "readxl"
))
```

## Business Value
This project demonstrates a practical business analytics workflow that goes beyond plotting a time series. It shows how to structure a forecasting problem, compare alternative methods, quantify uncertainty, and communicate results in a format that stakeholders can use for planning and market interpretation.

For employers, the project highlights skills in forecasting, model evaluation, data preparation, analytical storytelling, and repository organization.

## Future Enhancements
Potential improvements for the next version of the project:
- Add train/test split documentation and forecast accuracy metrics in the README
- Include residual diagnostics for the final shortlisted models
- Add an R Markdown or Quarto report for a more polished end-to-end presentation
- Enrich the analysis with external explanatory variables such as mortgage rates, inventory levels, or macroeconomic indicators
- Publish an interactive dashboard version of the results

## Notes
- The source data originates from FRED and may be subject to FRED terms of use.
- The chart image included in this repository presents the top three models as `ETS_MAM`, `ARIMA_man`, and `TSLM_seas`.
- The final forecast table used in this project includes monthly forecasts from June 2026 through May 2027.
