test_that("eval_code() works", {
  x <- eval_code(expression(1 + 1))
  
  expect_equal(x, 2)
  
  expect_message(eval_code(expression(stop())), "there was an error")
  expect_message(eval_code(expression(warning())), "there was a warning")
})

