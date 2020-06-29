context("prompts")

test_that("prompt to add dummy input list", {
  x_rmd <- 
    capture_messages(
      validate_inputs("demo-rmd-no-input-list.Rmd")
    )
  
  x_r <- 
    capture_messages(
      validate_inputs("demo-r-server-no-input-list.R")
    )
  
  expect_true(grepl("Add this code chunk", x_rmd[2]))
  expect_true(grepl("Add this code to your R file", x_r[2]))
})


test_that("prompts to update input list", {
  x_rmd <- 
    capture_messages(
      validate_inputs("demo-rmd-some-inputs.Rmd")
    )
  
  x_r <- 
    capture_messages(
      validate_inputs("demo-r-server-some-inputs.R")
    )
  
  expect_equal(x_rmd, x_r)
  expect_true(grepl("Update code", x_rmd[2]))
  expect_true(grepl("Update code", x_r[2]))
})


test_that("no prompt for input list because it already exists", {
  expect_null(
    capture_message(
      validate_inputs("demo-r-runapp-list.R")
    )
  )
  expect_null(
    capture_message(
      validate_inputs("demo-rmd-full.Rmd")
    )
  )
})


test_that("no prompt for input list because not reactive", {
  expect_null(
    capture_message(
      validate_inputs("demo-rmd-not-reactive.Rmd")
    )
  )
})
