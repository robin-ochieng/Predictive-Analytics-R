# modules/linePlotModule.R

LinePlotUI <- function(id) {
  ns <- NS(id)
    bs4Card(
        title = "Predicted Sales by Day",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("linePlot"))
      )
}

LinePlotServer <- function(id, forecastData) {
  moduleServer(id, function(input, output, session) {
    
    output$linePlot <- renderPlotly({
      req(forecastData())
      forecast <- forecastData()
      
      plot_ly(
        forecast,
        x = ~Date,
        y = ~PredictedSales, 
        type = 'scatter', 
        mode = 'lines+markers+text',
        text = ~round(PredictedSales),
        textposition = 'top center',
        line = list(color = '#1CA4F8'),
        marker = list(color = '#0d6efd')
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Count of Sales", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
    })
    
  })
}
