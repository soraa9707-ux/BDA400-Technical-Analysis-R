# BDA400 Assignment 6: Technical Analysis Using R, Visualization Stage
# Student: Piyush Tailor
# Interactive Shiny dashboard for stock prices, technical indicators, and crossover signals.

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Please install these packages first: ",
    paste(missing_packages, collapse = ", "),
    ". Run install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")) in the R console."
  )
}

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

to_stock_frame <- function(x) {
  data.frame(
    Date = as.Date(zoo::index(x)),
    Open = as.numeric(quantmod::Op(x)),
    High = as.numeric(quantmod::Hi(x)),
    Low = as.numeric(quantmod::Lo(x)),
    Close = as.numeric(quantmod::Cl(x)),
    Volume = as.numeric(quantmod::Vo(x))
  )
}

add_indicators <- function(df) {
  df$MA20 <- as.numeric(TTR::SMA(df$Close, n = 20))
  df$MA50 <- as.numeric(TTR::SMA(df$Close, n = 50))
  df$RSI <- as.numeric(TTR::RSI(df$Close, n = 14))

  macd_values <- TTR::MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9)
  df$MACD <- as.numeric(macd_values[, "macd"])
  df$MACD_Signal <- as.numeric(macd_values[, "signal"])

  previous_short <- dplyr::lag(df$MA20)
  previous_long <- dplyr::lag(df$MA50)
  df$Signal <- "Hold"

  buy <- !is.na(df$MA20) & !is.na(df$MA50) &
    !is.na(previous_short) & !is.na(previous_long) &
    previous_short <= previous_long & df$MA20 > df$MA50
  sell <- !is.na(df$MA20) & !is.na(df$MA50) &
    !is.na(previous_short) & !is.na(previous_long) &
    previous_short >= previous_long & df$MA20 < df$MA50

  df$Signal[buy] <- "Buy"
  df$Signal[sell] <- "Sell"
  df
}

ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock symbol (Yahoo Finance)", value = "AAPL"),
      dateRangeInput(
        "date_range", "Select date range",
        start = Sys.Date() - 365, end = Sys.Date(),
        min = "2000-01-01", max = Sys.Date()
      ),
      selectInput(
        "time_frame", "Time frame",
        choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"
      ),
      selectInput(
        "chart_type", "Price chart type",
        choices = c("Line", "Area", "Candlestick"), selected = "Line"
      ),
      checkboxGroupInput(
        "indicators", "Show indicators",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      helpText("Examples: AAPL, MSFT, GOOGL. Internet access is required.")
    ),
    mainPanel(
      h4(textOutput("status")),
      plotOutput("price_chart", height = "430px"),
      conditionalPanel(
        condition = "input.indicators.includes('RSI')",
        h4("Relative Strength Index (RSI)"),
        plotOutput("rsi_chart", height = "220px")
      ),
      conditionalPanel(
        condition = "input.indicators.includes('MACD')",
        h4("Moving Average Convergence Divergence (MACD)"),
        plotOutput("macd_chart", height = "220px")
      ),
      h4("Recent trading signals"),
      tableOutput("signals_table"),
      p("Signals are rule-based educational examples, not investment advice.")
    )
  )
)

server <- function(input, output, session) {
  stock_data <- reactive({
    req(nzchar(trimws(input$symbol)), input$date_range)

    validate(
      need(input$date_range[1] <= input$date_range[2],
           "The start date must be on or before the end date.")
    )

    tryCatch({
      raw <- quantmod::getSymbols(
        toupper(trimws(input$symbol)),
        src = "yahoo",
        from = as.Date(input$date_range[1]) - 120,
        to = as.Date(input$date_range[2]) + 1,
        auto.assign = FALSE,
        warnings = FALSE
      )
      if (NROW(raw) == 0) stop("No data was returned for this symbol.")

      if (input$time_frame == "Weekly") {
        raw <- quantmod::to.weekly(raw, indexAt = "lastof", OHLC = TRUE)
      } else if (input$time_frame == "Monthly") {
        raw <- quantmod::to.monthly(raw, indexAt = "lastof", OHLC = TRUE)
      }

      df <- to_stock_frame(raw)
      df <- df[
        df$Date >= as.Date(input$date_range[1]) &
          df$Date <= as.Date(input$date_range[2]),
        , drop = FALSE
      ]
      validate(need(nrow(df) > 0, "No prices found in the selected date range."))
      add_indicators(df)
    }, error = function(e) {
      validate(need(FALSE, paste("Could not fetch stock data:", conditionMessage(e))))
    })
  })

  output$status <- renderText({
    paste("Symbol:", toupper(trimws(input$symbol)),
          "| Time frame:", input$time_frame)
  })

  output$price_chart <- renderPlot({
    df <- stock_data()
    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), linewidth = 0.7)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), alpha = 0.25) +
        geom_line(aes(y = Close), linewidth = 0.5)
    } else {
      candle_width <- if (input$time_frame == "Daily") 0.7 else
        if (input$time_frame == "Weekly") 3.5 else 20
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High), linewidth = 0.35) +
        geom_rect(
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          colour = "grey30", linewidth = 0.2
        ) +
        scale_fill_manual(
          values = c("TRUE" = "darkgreen", "FALSE" = "firebrick"),
          guide = "none"
        )
    }

    if ("Moving Averages" %in% input$indicators) {
      p <- p +
        geom_line(aes(y = MA20, colour = "MA 20"), linewidth = 0.65, na.rm = TRUE) +
        geom_line(aes(y = MA50, colour = "MA 50"), linewidth = 0.65, na.rm = TRUE) +
        scale_colour_manual(values = c("MA 20" = "blue", "MA 50" = "orange"),
                            name = "Indicator")
    }

    signal_points <- df[df$Signal %in% c("Buy", "Sell"), , drop = FALSE]
    if (nrow(signal_points) > 0) {
      p <- p +
        geom_point(data = signal_points, aes(y = Close, shape = Signal), size = 3) +
        geom_text(data = signal_points, aes(y = Close, label = Signal),
                  vjust = -0.8, size = 3, show.legend = FALSE) +
        scale_shape_manual(values = c("Buy" = 24, "Sell" = 25))
    }

    p + labs(
      title = paste(toupper(trimws(input$symbol)), "stock price"),
      x = "Date", y = "Price (USD)"
    ) + theme_minimal()
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$indicators)
    df <- stock_data()
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "RSI (14)", x = "Date", y = "RSI") +
      theme_minimal()
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$indicators)
    df <- stock_data()
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = MACD, colour = "MACD"), na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal, colour = "Signal"), na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_colour_manual(values = c("MACD" = "blue", "Signal" = "orange"),
                          name = "Line") +
      labs(title = "MACD (12, 26, 9)", x = "Date", y = "Value") +
      theme_minimal()
  })

  output$signals_table <- renderTable({
    df <- stock_data()
    df %>%
      filter(Signal %in% c("Buy", "Sell")) %>%
      select(Date, Close, MA20, MA50, RSI, MACD, Signal) %>%
      arrange(desc(Date)) %>%
      head(10)
  }, digits = 2)
}

shinyApp(ui = ui, server = server)
