# valueboxesModule.R

# 1) UI portion: defines how the boxes appear
valueBoxesModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    valueBoxOutput(ns("totalSales"), width = 4),
    valueBoxOutput(ns("avgSales"), width = 4),
    valueBoxOutput(ns("daysForecasted"), width = 4)
  )
}

# 2) Server portion: renders the three value boxes
valueBoxesModuleServer <- function(id, forecastData, startDate, endDate) {
  moduleServer(id, function(input, output, session) {
    
    # 1) Total Sales
    output$totalSales <- renderValueBox({
      req(forecastData())
      data <- forecastData()
      valueBox(
        value = formatKES(sum(data$PredictedSales)),
        subtitle = "Daily Sales Count",
        icon = NULL,  # or icon("dollar-sign") if you want a Font Awesome icon
        color = "white"
      )
    })
    
    # 2) Average Sales
    output$avgSales <- renderValueBox({
      req(forecastData())
      data <- forecastData()
      valueBox(
        value = formatKES(mean(data$PredictedSales)),
        subtitle = "Average Daily Sales",
        icon = NULL,  # or icon("chart-line")
        color = "white"
      )
    })
    
    # 3) Days Forecasted
    output$daysForecasted <- renderValueBox({
      req(startDate(), endDate())
      days_count <- as.integer(difftime(endDate(), startDate(), units = "days")) + 1
      valueBox(
        value = paste(days_count, "Days"),
        subtitle = "Forecast Duration",
        icon = NULL,  # or icon("calendar-alt")
        color = "white"
      )
    })
    
  })
}
