app_guts <-
  'ui = fluidPage(
      numericInput("n", "n", 20),
      plotOutput("plot")
    ),
    server = function(input, output) {
      data <- reactive(head(cars, input$n))
      output$plot <- renderPlot(plot(data()))
    }'

runapp_has_shinyapp <- parse(text = paste0("runApp(shinyApp(", app_guts, "))"))

runapp_has_list <- parse(text = paste0("runApp(list(", app_guts, "))"))

shinyapp_alone <- parse(text = paste0("shinyApp(", app_guts, ")"))

shinyapp_assigned <- parse(text = paste0("app <- shinyApp(", app_guts, ")"))


test_that("assingments found", {
  expected <- c(
    "data <- reactive(head(cars, input$n))",
    "output$plot <- renderPlot(plot(data()))"
  )

  expect_equal(
    expected,
    extract_from_app_fn(runapp_has_shinyapp),
    extract_from_app_fn(runapp_has_list),
    extract_from_app_fn(shinyapp_alone),
    extract_from_app_fn(shinyapp_assigned)
  )
})

test_that("warning if no server listed", {
  expect_warning(
    extract_from_app_fn("1+1"),
    "server not listed"
  )
})


test_that("server found", {
  expect_equal(
    names(inside_runapp(runapp_has_shinyapp)),
    names(inside_runapp(runapp_has_list)),
    names(inside_shinyapp(shinyapp_alone)),
    names(inside_shinyapp(shinyapp_assigned)),
    c("", "ui", "server")
  )
})


test_that("guts extracts args", {
  x <- "mean(list(1:10))"
  
  res_mean <- invisible(guts(x, "mean"))
  res_list <- invisible(guts(x, "mean") %>% guts("list"))
  
  expect_equal(res_mean[[2]], quote(list(1:10)))
  expect_equal(res_list[[2]], quote(1:10))
})
