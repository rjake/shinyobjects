# input_usage()----
test_that("R files look for dummy_input list", {
  code <- "x <- input$a;"
  expected <- 
    tibble::tibble(
      input_name = "a",
      times_used = 1L,
      lines = glue::glue("1")
    )
  
  tmp <- tempfile("data")
  write(code, tmp)
  actual <- input_usage(file = tmp)
  unlink(tmp)
  
  expect_equal(actual, expected)
})


# find_input_code() ----
test_that("R files look for dummy_input list", {
  code <- "dummy_input <- list(x = 1); y = 2;"
  expected <- "input <- list(x = 1)"
  
  tmp <- tempfile("data", fileext = ".R")
  write(code, tmp)
  actual <- find_input_code(file = tmp)
  unlink(tmp)
  
  expect_equal(actual, expected)
})


# validate_inputs()----
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
  expect_message(
    capture_message(
      validate_inputs("demo-r-runapp-list.R")
    ),
    regexp = NA
  )
  expect_message(
    capture_message(
      validate_inputs("demo-rmd-full.Rmd")
    ),
    regexp = NA
  )
})


test_that("no prompt for input list because not reactive", {
  expect_message(
    capture_message(
      validate_inputs("demo-rmd-not-reactive.Rmd")
    ),
    regexp = NA
  )
})
