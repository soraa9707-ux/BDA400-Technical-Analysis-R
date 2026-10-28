# BDA400 Assignment 6 — Technical Analysis Using R, Visualization Stage

Student: Piyush Tailor

This repository contains an R Shiny dashboard that fetches historical stock prices from Yahoo Finance, plots prices, displays SMA/RSI/MACD indicators, and identifies 20-period/50-period moving-average crossover signals.

## Files
- `app.R` — complete Shiny application.
- `docs/implementation_notes.md` — overview and run instructions.

## Requirements
Install R and RStudio, then run this in the R console:

```r
install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr", "zoo"))
```

## Run the app
1. Download or clone this repository.
2. Open `app.R` in RStudio.
3. Click **Run App** (or run `shiny::runApp()` from the folder containing `app.R`).
4. Enter a Yahoo Finance ticker such as `AAPL`, choose dates, timeframe, chart type, and indicators.

An internet connection is required to fetch stock data. If a ticker or date range is unavailable, the app will show an error message.

## Before submission
- Upload `app.R` and `README.md` to a public GitHub repository.
- Add the actual repository URL to the cover page of your PDF.
- Submit the cover page/PDF to the LMS as directed by your instructor.
- Run the app on your computer and capture genuine screenshots of its output.

Trading signals are educational examples and are not financial advice.
