# dataPreparationModule.R
modDataPrepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # -- 1) Load your data
    #    (Adjust paths or code as needed; you can also make them configurable)
    sales_data <- read_csv(
      "./data/Data.csv",
      col_types = cols(Date = col_date(format = "%m/%d/%Y"), Sales = col_number())
    )
    
    # -- 2) Preprocess the data
    sales_data <- sales_data %>%
      mutate(Date = ymd(Date)) %>%
      arrange(Date) %>%
      mutate(Sales = as.numeric(gsub(",", "", Sales, fixed = TRUE)))
    
    # Aggregate sales data to daily frequency
    daily_sales <- sales_data %>%
      group_by(Date = floor_date(Date, unit = "day")) %>%
      summarise(Sales = sum(!is.na(Sales))) %>%
      filter(Sales != 0) %>%
      drop_na()
    
    # Calculate IQR and define bounds
    Q1 <- quantile(daily_sales$Sales, 0.15)
    Q3 <- quantile(daily_sales$Sales, 0.85)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    # Remove Outliers
    daily_sales <- daily_sales %>%
      filter(Sales >= lower_bound & Sales <= upper_bound)
    
    # Adding time-based features
    daily_sales <- daily_sales %>%
      mutate(
        year = year(Date), 
        month = month(Date),
        day = day(Date), 
        weekday = wday(Date),
        quarter = quarter(Date)
      )
    
    # Creating lag features
    for (i in 1:31) {
      daily_sales <- daily_sales %>% mutate(!!paste0("sales_lag_", i) := lag(Sales, i))
    }
    
    # Remove rows with missing values
    daily_sales <- drop_na(daily_sales)
    
    # We return daily_sales as a reactive expression
    # so other modules or the main server can use it.
    # We'll store it in a reactiveVal container
    dailySalesRV <- reactiveVal(daily_sales)
    
    # Return the reactiveVal
    return(dailySalesRV)
  })
}
