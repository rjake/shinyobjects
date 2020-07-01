test_that("extract from curly braces", {
  actual <- code_between("test {x {y}}", "c")
  expected <- "x {y}"
  expect_equal(actual, expected)
})


test_that("extract from parentheses", {
  actual <- code_between("test {x (y)}", "p")
  expected <- "y"
  expect_equal(actual, expected)
})
