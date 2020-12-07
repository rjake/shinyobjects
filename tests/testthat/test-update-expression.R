# test_that("output list renderText", {
#   e <- new.env()
#   e$output <- list()
#   
#   x <- expr(output$txt <- renderText({10}))
#   
#   eval(update_expressions(x), envir = e)
#   
#   expect_equal(
#     object = class(e$output$txt), 
#     expected = "numeric"
#   )
# })


# if (interactive()) {
#   test_that("output list renderPlot", {
#     e <- new.env()
#     e$output <- list()
#     
#     x <- expr(output$plot <- renderPlot({plot(cars)}))
# 
#     plot(cars)
#     
#     eval(update_expressions(x), envir = e)
# 
#     expect_equal(
#       object = class(e$output$plot),
#       expected = "recordedplot"
#     )
#   })
# }

test_that("output list renderTable", {
  e <- new.env()
  e$output <- list()
  
  x <- expr(output$table <- renderTable({invisible(head(cars))}))
  eval(update_expressions(x), envir = e)
  
  expect_equal(
    object = class(e$output$table), 
    expected = "data.frame"
  )
})



test_that("updates reactiveValues to list", {
  code <- expr(y <- reactiveValues(a = 1, b = 2))
  new_code <- update_expressions(code)
  expect_equal(
    object = deparse(new_code),
    expected = "y <- list(a = 1, b = 2)"
  )
})



test_that("updates reactive to function", {
  code <- expr(y <- reactive({print(input$n)}))
  new_code <- update_expressions(code)
  actual <- paste(trimws(deparse(new_code)), collapse = "")
  expect_equal(
    object = actual,
    expected = "y <- function() {{print(input$n)}}"
  )
})



test_that("updates eventReactive to function", {
  code <- expr(y <- eventReactive(input$button, {print(input$n)}))
  new_code <- update_expressions(code)
  actual <- paste(trimws(deparse(new_code)), collapse = "")
  expect_equal(
    object = actual,
    expected = "y <- function() {{print(input$n)}}"
  )
})

