test_that("stop if 'c' or 'p' not given as argument", {
  expect_error(char_between("{.}", ""))
})

test_that("warn pattern is missing", {
  expect_warning(char_between("{.}"))
})
