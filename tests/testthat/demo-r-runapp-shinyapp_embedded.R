library(shiny)

# Apps can be run without a server.r and ui.r file
runApp(shinyApp(
  ui = fluidPage(
    numericInput("x", "# of obs.", 20),
    plotOutput("plot")
  ),
  server = function(input, output) {
    my_df <- reactive({
      head(cars, input$x)
    })
    
    about_df <- reactiveValues(n_obs = nrow(my_df()), len = length(my_df()))
    
    output$plot <- renderPlot(
      plot(my_df())
    )
  }
))

dummy_input <- list(x = 10, y = "Hello")
