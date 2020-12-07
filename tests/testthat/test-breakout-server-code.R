test_that("warn if multiple server <- ", {
 code <- "server <- 1; server <- 2;"
 tmp <- tempfile("data")
 write(code, tmp)
 expect_warning(breakout_server_code(file = tmp)) 
 unlink(tmp)
})


test_that("use everything when missing server <- ", {
  code <- "a <- 1; b <- 2;"
  expected <- parse_exprs("a <- 1; b <- 2")

  tmp <- tempfile("data")
  write(code, tmp)
  actual <- breakout_server_code(file = tmp)
  unlink(tmp)
  
  expect_equal(
    deparse(expected), 
    deparse(actual)
  )
})

test_that("finds all assignments", {
  assignments <- 
    breakout_server_code("demo-r-runapp-list.R") %>%
    find_all_assignments_r() %>%
    convert_assignments()
  
  expect_equal(length(assignments), 4)
})

