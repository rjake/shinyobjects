test_that("output list renderText", {
  e <- new.env()
  e$output <- list()
  
  x <- expression(output$txt <- renderText({10}))
  
  eval(update_expressions(x), envir = e)
  
  expect_equal(
    object = class(e$output$txt), 
    expected = "numeric"
  )
})


if (interactive()) {
  test_that("output list renderPlot", {
    e <- new.env()
    e$output <- list()
    
    x <- expression(output$plot <- renderPlot({plot(cars)}))

    eval(update_expressions(x), envir = e)

    expect_equal(
      object = class(e$output$plot),
      expected = "recordedplot"
    )
  })
}

test_that("output list renderTable", {
  e <- new.env()
  e$output <- list()
  
  x <- expression(output$table <- renderTable({invisible(head(cars))}))
  eval(update_expressions(x), envir = e)
  
  expect_equal(
    object = class(e$output$table), 
    expected = "data.frame"
  )
})



test_that("updates reactiveValues to list", {
  code <- expression(y <- reactiveValues(a = 1, b = 2))
  new_code <- update_expressions(code)
  expect_equal(
    object = deparse(new_code),
    expected = "expression(y <- list(a = 1, b = 2))"
  )
})



test_that("updates reactive to function", {
  code <- expression(y <- reactive({print(input$n)}))
  new_code <- update_expressions(code)
  actual <- paste(trimws(deparse(new_code)), collapse = "")
  expect_equal(
    object = actual,
    expected = "expression(y <- function() {print(input$n)})"
  )
})



test_that("updates eventReactive to function", {
  code <- expression(y <- eventReactive(input$button, {print(input$n)}))
  new_code <- update_expressions(code)
  actual <- paste(trimws(deparse(new_code)), collapse = "")
  expect_equal(
    object = actual,
    expected = "expression(y <- function() {print(input$n)})"
  )
})
