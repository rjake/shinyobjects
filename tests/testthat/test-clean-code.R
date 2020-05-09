library(testthat)
library(shinyobjects)

test_that("shiny::reactive() and reactive() both work", {
  no_namespace <- "test <- shiny::reactive(abcd"
  with_namespace <- "test <- reactive(abcd"
  
  expect_equal(
    convert_assignments(no_namespace),
    convert_assignments(with_namespace)
  )        
})


test_that("strings_to_find() most recent list", {
  expect_equal(
    strings_to_find(),
    "^(library|[\\w\\.\\$0:9]+ (<-|=[^=]))"
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
  
  expect_equal(length(assignments), 2)
})


test_that("find all assignments rmd", {
  assignments <- find_all_assignments_rmd("demo-rmd-full.Rmd")
  expect_equal(length(assignments), 2)
})


test_that("code_to_df", {
  x <- "a <- reactive(x)"
  actual <- code_to_df(x)
  
  expected <-
    tibble::tibble(
      raw = x,
      code = "a <- function() (x)"
    )

  expect_equal(actual, expected)
})


test_that("find input code", {
  inputs_rmd <- find_input_code("demo-rmd-full.Rmd")
  inputs_r_runapp <- find_input_code("demo-r-runapp-full.R")
  inputs_r_server <- find_input_code("demo-r-server-full.R")
  
  expect_equal(
    inputs_rmd,
    inputs_r_runapp,
    inputs_r_server,
    "input <- list(x = 1, y = 2)"
  )
})
