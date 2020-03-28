# Apps can be run without a server.r and ui.r file
runApp(list(
  ui = fluidPage(
    numericInput("n", "n", 20),
    plotOutput("plot")
  ),
  server = function(input, output) {
    data <- reactive(head(cars, input$n))
    output$plot <- renderPlot(plot(data()))
  }
))

dummy_input <- list(n = 20)
