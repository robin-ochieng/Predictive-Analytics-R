# forecastingModule.R
modForecastServer <- function(id, dailySalesReactive, startDateReactive, endDateReactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1) Train XGBoost model once, using the daily_sales data from module 1
    model <- reactive({
      req(dailySalesReactive())
      daily_sales <- dailySalesReactive()
      
      X <- dplyr::select(daily_sales, -c(Sales, Date))
      y <- daily_sales$Sales
      
      xgboost::xgboost(
        data = as.matrix(X),
        label = y,
        nrounds = 1000,
        objective = "reg:squarederror",
        max_depth = 5,
        eta = 0.1,
        nthread = 1,
        verbose = 0
      )
    })
    
    # 2) Compute a forecast based on date inputs from the main UI
    forecastData <- reactive({
      req(startDateReactive(), endDateReactive())
      
      future_dates <- seq(as.Date(startDateReactive()), as.Date(endDateReactive()), by = "day")
      
      future_data <- data.frame(
        Date     = future_dates,
        year     = lubridate::year(future_dates),
        month    = lubridate::month(future_dates),
        day      = lubridate::day(future_dates),
        weekday  = lubridate::wday(future_dates),
        quarter  = lubridate::quarter(future_dates)
      )
      
      # Borrow the tail of dailySalesReactive() to fill lag columns
      daily_sales <- dailySalesReactive()
      for (i in 1:31) {
        future_data[[paste0("sales_lag_", i)]] <-
          tail(daily_sales$Sales, 31 + i - 1)[1:length(future_dates)]
      }
      
      # Predict
      xgb_model <- model()
      future_data$PredictedSales <- predict(xgb_model, as.matrix(future_data[, -1]))
      
      future_data %>% dplyr::select(Date, PredictedSales)
    })
    
    # Return the reactive with the forecast data
    return(forecastData)
  })
}
