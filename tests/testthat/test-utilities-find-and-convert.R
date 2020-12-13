suppressWarnings(library(shiny))
suppressWarnings(library(rlang))

# find_all_assignments_r() ----


# find_all_assignments_rmd() ----
test_that("find all assignments rmd", {
  assignments <- find_all_assignments_rmd("demo-rmd-full.Rmd")
  expect_equal(length(assignments), 5)
})



# update_expressions() ----  
if (interactive()) {
  test_that("output list renderPlot", {
    e <- new.env()
    e$output <- list()

    x <- expr(output$plot <- renderPlot({plot(cars)}))

    plot(cars)

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


test_that("updates withProgress", {
  code <- expr(y <- reactive({
      req(TRUE)
      withProgress(print(123))
    })
  )
  
  new_code <- update_expressions(code)
  
  expect_true(code != new_code)
  expect_equal(
    paste(trimws(deparse(new_code)), collapse = ""),
    "y <- function() {{req(TRUE)print(123)}}"
  )
})


test_that("updates reactiveValuesToList to list", {
  # options(shiny.suppressMissingContextError = TRUE)
  code <- expr(y <- reactiveValuesToList(list(a = 123)))
  new_code <- update_expressions(code)
  expect_equal(
    object = deparse(new_code),
    expected = "y <- as.list(list(a = 123))"
  )
})



test_that("updates reactiveVal modifies in place", {
  # is updated
  code <- expr(y <- reactiveVal())
  new_code <- update_expressions(code)
  expect_true(code != new_code)
  # modifie in place
  test_val <- eval(new_code)
  test_val(10)
  expect_equal(test_val(), 10)
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



# convert_assignments() ----
test_that("shiny::reactive() and reactive() both work", {
  no_namespace <- exprs(test <- shiny::reactive(123))
  with_namespace <- exprs(test <- reactive(123))
  
  expect_equal(
    convert_assignments(no_namespace),
    convert_assignments(with_namespace)
  )        
})


test_that("shiny::reactiveValues() and reactiveValues() both work", {
  no_namespace <- exprs(test <- shiny::reactiveValues(a = 1, b = 2))
  with_namespace <- exprs(test <- reactiveValues(a = 1, b = 2))
  
  expect_equal(
    convert_assignments(no_namespace),
    convert_assignments(with_namespace)
  )        
})


test_that("assignments can be = or <-", {
  x <- c("a", "a = 1", "b == 2", "c <- 3")
  expect_equal(
    find_all_assignments_r(x),
    x[c(2,4)]
  )
})


test_that("find input code", {
  inputs_rmd <- find_input_code("demo-rmd-full.Rmd")
  inputs_r_runapp <- find_input_code("demo-r-runapp-list.R")
  inputs_r_server <- find_input_code("demo-r-server-full.R")
  
  expect_equal(
    inputs_rmd,
    inputs_r_runapp,
    inputs_r_server,
    "input <- list(x = 1, y = 2)"
  )
})


