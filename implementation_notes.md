# Implementation Notes — Assignment 6

## Dashboard features
- Retrieves historical OHLCV data from Yahoo Finance.
- Supports Daily, Weekly, and Monthly intervals.
- Offers Line, Area, and Candlestick price charts.
- Calculates SMA(20), SMA(50), RSI(14), and MACD(12, 26, 9).
- Allows Moving Averages, RSI, and MACD to be toggled.
- Marks Buy when SMA(20) crosses above SMA(50), and Sell when it crosses below.
- Lists recent crossover events in a table.

## Run
1. Install R and RStudio.
2. Install required packages: `install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr", "zoo"))`.
3. Open `app.R` in RStudio and click **Run App**.
4. Use a valid Yahoo Finance ticker and an available date range.

## Evidence to capture yourself
Add screenshots of the app running, chart types, enabled indicators, and the signal table. Do not use placeholders as proof that the app ran.
