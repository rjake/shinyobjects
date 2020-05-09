# Apps can be run without a server.r and ui.r file
runApp(list(
  ui = fluidPage(
    numericInput("n", "n", 20),
    plotOutput("plot")
  ),
  server = function(input, output) {
    data <- reactive(head(cars, input$x))
    output$y <- renderText(input$y)
  }
))

dummy_input <- list(x = 1, y = 2)
