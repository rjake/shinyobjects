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
