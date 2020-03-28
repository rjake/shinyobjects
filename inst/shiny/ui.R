ui <- fluidPage(
  
  tabsetPanel(
    
    # OPCIONES
    tabPanel(
      
      "1. Opciones",
      br(),
      h2("Selecciona los datos que quieres representar"),
      selectInput(
        inputId = "iris.species",
        label = "Especies",
        choices = c()
      ),
      
      hr(),
      h2("Datos a comparar"),
      fluidRow(
        column(
          6,
          selectInput(
            inputId = "iris.section.1",
            label = "Eje X",
            choices = c()
          )
        ),
        column(
          6,
          selectInput(
            inputId = "iris.section.2",
            label = "Eje Y",
            choices = c()
          )
        )
        
      )
    ),
    
    # GRÁFICO
    tabPanel(
      "2. Gráfico",
      h2("Selecciona en el gráfico el rango de datos que quieres ver con más detalles"),
      plotOutput(
        outputId = "iris.plot",
        brush = brushOpts(
          id = "iris.plot.brush",
          fill = "#9cf",
          stroke = "#036",
          opacity = 0.25,
          delay = 300,
          delayType = "debounce",
          clip = TRUE,
          direction = "xy",
          resetOnNew = FALSE
        )
      )
    ),
    
    # DATOS
    tabPanel(
      "3. Datos",
      h2("Datos seleccionados mediante los filtros anteriores"),
      dataTableOutput(
        outputId = "iris.table"
      )
    )
    
  )
  
)