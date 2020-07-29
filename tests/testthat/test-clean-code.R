test_that("shiny::reactive() and reactive() both work", {
  no_namespace <- parse(text = "test <- shiny::reactive(123)")
  with_namespace <- parse(text = "test <- reactive(123)")
  
  expect_equal(
    convert_assignments(no_namespace),
    convert_assignments(with_namespace)
  )        
})


test_that("shiny::reactiveValues() and reactiveValues() both work", {
  no_namespace <- parse(text = "test <- shiny::reactiveValues(a = 1, b = 2)")
  with_namespace <- parse(text = "test <- reactiveValues(a = 1, b = 2)")
  
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


test_that("find all assignments r", {
  assignments <- 
    breakout_server_code("demo-r-server-some-inputs.R") %>% 
    find_all_assignments_r()
  
  expect_equal(length(assignments), 4)
})


test_that("find all assignments rmd", {
  assignments <- find_all_assignments_rmd("demo-rmd-full.Rmd")
  expect_equal(length(assignments), 5)
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

