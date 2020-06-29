server <- function(input, output) {
  my_df <- reactive({
    head(cars, input$x)
  })
  
  output$plot <- renderPlot(
    plot(my_df())
  )
}