library(shiny)

server <- function(input, output) {
  my_df <- reactive({
    head(cars, input$x)
  })
  
  about_df <- reactiveValues(n_obs = nrow(my_df()), len = length(my_df()))
  
  output$plot <- renderPlot(
    plot(my_df())
  )
}