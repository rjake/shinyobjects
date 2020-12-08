library(shiny)

print(getwd())
# Apps can be run without a server.r and ui.r file
runApp(
  list(
    ui = fluidPage(
      numericInput("x", "# of obs.", 20),
      tableOutput("df")
  ),
  server = function(input, output) {
    my_df <- reactive({
      head(cars, input$x)
    })
    
    about_df <- reactiveValues(n_obs = nrow(my_df()), len = length(my_df()))
    
    
    output$df <- renderTable(
      my_df()
    )
  }
  ))

dummy_input <- list(x = 10, y = "Hello")
