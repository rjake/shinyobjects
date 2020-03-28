test_that("R files look for dummy_input list", {
  code <- "dummy_input <- list(x = 1); y = 2;"
  expected <- "input <- list(x = 1)"
  
  tmp <- tempfile("data", fileext = ".R")
  write(code, tmp)
  actual <- find_input_code(file = tmp)
  unlink(tmp)
  
  expect_equal(actual, expected)
})
