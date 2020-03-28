test_that("stop if multiple server <- ", {
 code <- "server <- 1; server <- 2;"
 tmp <- tempfile("data")
 write(code, tmp)
 expect_error(breakout_server_code(file = tmp)) 
 unlink(tmp)
})


test_that("use everything when missing server <- ", {
  code <- "a <- 1; b <- 2;"
  expected <- c("a <- 1", "b <- 2")

  tmp <- tempfile("data")
  write(code, tmp)
  actual <- breakout_server_code(file = tmp)
  unlink(tmp)
  
  expect_equal(expected, actual)
})

test_that("warning if missing server <- ", {
  code <- "x <- 1; server <- function() {y <- reactive({1})}; z <- x;"
  expected <- c("x <- 1", "y <- function() ({ 1 })", "z <- x")
  
  tmp <- tempfile("data")
  write(code, tmp)
  actual <- 
    breakout_server_code(file = tmp) %>% 
    gsub("\\s{2,}", " ", .) 
  unlink(tmp)

  expect_equal(actual, expected)
})

