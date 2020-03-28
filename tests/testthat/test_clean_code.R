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


test_that("find_all_assignments_r", {
  assignments <- 
    breakout_server_code("demo_r_file_server.R") %>% 
    find_all_assignments_r()
  
  expect_equal(length(assignments), 2)
})


test_that("find_all_assignments_rmd", {
  assignments <- find_all_assignments_rmd("demo_rmd_file.Rmd")
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


test_that("find_input_code", {
  inputs <- find_input_code("demo_rmd_file.Rmd")
  expect_equal(
    inputs, 
    "input <- list(x = 1, y = 2)"
  )
})
