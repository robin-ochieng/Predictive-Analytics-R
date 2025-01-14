# modules/tableModule.R

predictionTableUI <- function(id) {
  ns <- NS(id)
      bs4Card(
        title = "Sales Prediction Table",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        DTOutput(ns("predictionsTable"))
      )
}

predictionTableServer <- function(id, forecastData) {
  moduleServer(id, function(input, output, session) {
    
    output$predictionsTable <- DT::renderDT({
      req(forecastData())
      forecast <- forecastData()
      
      # Create a new column showing the day of the week
      forecast[["Day of Week"]] <- weekdays(forecast$Date)
      
      # Format the Date column
      forecast$Date <- format(forecast$Date, "%Y-%m-%d")
      
      # Add a formatted Sales Count
      forecast[["Sales Count"]] <- scales::comma(forecast$PredictedSales, accuracy = 1)
      
      # Drop the numeric column (PredictedSales) if you only want the formatted version
      forecast <- forecast[, c("Date", "Day of Week", "Sales Count")]
      
      datatable(
        forecast,
        options = list(
          pageLength = 14,
          autoWidth = FALSE,
          lengthChange = FALSE,
          paging = TRUE,
          searching = FALSE,
          info = FALSE,
          initComplete = JS("
            function(settings, json) {
              $(this.api().table().header()).css({
                'background-color': '#FFFFFF',
                'color': '#000000'
              });
            }
          ")
        )
      )
    }, server = FALSE)
    
  })
}
