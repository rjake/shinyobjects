## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, eval = FALSE)

input <-list(
  displ = 1.8,
  year = 2008,
  drv = "f"
)

## ----show_plot----------------------------------------------------------------
#  library(tidyverse)
#  library(shiny)
#  
#  raw_data <- mpg
#  
#  renderPlot({
#    df <-
#      raw_data %>%
#      filter(
#        displ >= input$displ,
#        year == input$year,
#        drv == input$drv
#      )
#  
#    ggplot(df, aes(class)) +
#      geom_bar()
#  })

## -----------------------------------------------------------------------------
#  reactive_df <- reactive(
#    raw_data %>%
#      filter(displ >= input$displ)
#  )
#  
#  renderPlot(
#    ggplot(reactive_df(), aes(class)) +
#      geom_bar()
#  )

## -----------------------------------------------------------------------------
#  renderPlot({
#    df <-
#      raw_data %>%
#      filter(displ >= input$displ)
#  
#    ggplot(df, aes(class)) + geom_bar()
#  })

