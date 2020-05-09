server = function(input, output) {
  data <- reactive(head(cars, input$x))
  output$y <- renderText(input$y)
}

dummy_input <- list(x = 1, y = 2)
