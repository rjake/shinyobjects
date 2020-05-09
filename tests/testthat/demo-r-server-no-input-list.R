server = function(input, output) {
  data <- reactive(head(cars, input$x))
  output$y <- renderText(input$y)
}
