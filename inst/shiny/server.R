dummy_input <-
  list(
    #iris.plot.brush = ,
    iris.section.1 = "Petal.Length",
    iris.section.2 = "Sepal.Length",
    iris.species = "virginica"
  )
# input <- dummy_input

server <- function(input, output, session) {
  
  # Especies
  observe({
    updateSelectizeInput(
      session,
      inputId = "iris.species", 
      choices = sort(iris$Species)
    )
  })
  
  # Eje X
  observe({
    iris.names <- head(names(iris), -1)
    updateSelectizeInput(
      session,
      inputId = "iris.section.1", 
      choices = sort(iris.names)
    )
  })
  
  # Eje Y
  observe({
    
    iris.names <- head(names(iris), -1)
    iris.names.selected <- strsplit(input$iris.section.1, split = "[.]")[[1]][1]
    iris.type.selected <- strsplit(input$iris.section.1, split = "[.]")[[1]][2]
    
    iris.names.output <- NA
    if (!is.na(iris.type.selected)) { 
      if (iris.type.selected == "Width") {
        iris.names.output <- paste0(iris.names.selected, ".Length")
      } else {
        iris.names.output <- paste0(iris.names.selected, ".Width")
      }
    }
    
    updateSelectizeInput(
      session,
      inputId = "iris.section.2", 
      choices = sort(iris.names.output)
    )
    
  })
  
  output$iris.plot <- renderPlot({
    
    if ((!is.na(input$iris.section.1)) &&  (!is.na(input$iris.section.2))) {
      iris.data <- iris[iris$Species == input$iris.species,]
      with(
        iris[iris$Species == input$iris.species,], 
        plot(
          iris.data[,input$iris.section.1], 
          iris.data[,input$iris.section.2], 
          main = paste("Iris dataset from", input$iris.species), 
          xlab = input$iris.section.1,
          ylab = input$iris.section.2
        )
      )
    }
    
  })
  
  # Obtener datos seleccionados en el gráfico
  brushed_data <- reactive({
    brushedPoints(
      df = iris[iris$Species == input$iris.species,], 
      brush = input$iris.plot.brush,
      xvar = input$iris.section.1,
      yvar = input$iris.section.2,
      allRows = FALSE
    )
  })
  
  output$iris.table <- renderDataTable(
    brushed_data()
  )
  
}

a <- stop(error)
b <- warning("test")
