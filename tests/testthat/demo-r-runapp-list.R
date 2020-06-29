# Apps can be run without a server.r and ui.r file
runApp(list(
  ui = fluidPage(
    numericInput("x", "# of obs.", 20),
    plotOutput("plot")
  ),
  server = function(input, output) {
    my_df <- reactive({
      head(cars, input$x)
    })
    
    output$plot <- renderPlot(
      plot(my_df())
    )
  }
))

dummy_input <- list(x = 10, y = "Hello")
