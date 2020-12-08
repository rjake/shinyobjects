# breakout_server_code() ---- 
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


test_that("find all assignments r", {
  assignments <- 
    breakout_server_code("demo-r-server-some-inputs.R") %>% 
    find_all_assignments_r()
  
  expect_equal(length(assignments), 4)
})


# extract_from_server_assignment() ----


# extract_from_server_file() ----


# extract_from_app() ----


# update_code() ----
test_that("update code works", {
  x <- list(1, 2, 3)
  actual <- update_code(x, c(4, 4), 2)
  expected <- list(1, 4, 4, 3)
  expect_equal(actual, expected)
})


# is_server_file() ----


# server_is_assigned() ----

