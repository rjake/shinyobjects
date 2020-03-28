server = function(input, output) {
  data <- reactive(head(cars, input$n))
  output$plot <- renderPlot(plot(data()))
}

dummy_input <- list(n = 20)
