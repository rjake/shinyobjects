test_that("update code works", {
  x <- list(1, 2, 3)
  actual <- update_code(x, c(4, 4), 2)
  expected <- list(1, 4, 4, 3)
  expect_equal(actual, expected)
})
