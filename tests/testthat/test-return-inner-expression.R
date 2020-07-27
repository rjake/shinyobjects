test_that("return argument pulls returns right piece of code", {
  x <- expression(y <- eventReactive(input$button, {print(input$n)}))
  code_as_call <- as.call(x)[[1]]
  
  all_args <- full_argument_names(code_as_call[[3]])

  expect_equal(
    object = all_args, 
    expected = c("", "eventExpr", "valueExpr")
  )
  
  get_event <- return_inner_expression(code_as_call[[3]], "eventExpr")
  get_value <- return_inner_expression(code_as_call[[3]], "valueExpr")
  
  expect_equal(
    object = as.call(get_event), 
    expected = as.call(quote(input$button))
  )
  
  expect_equal(
    object = as.character(get_value)[[2]], 
    expected = "print(input$n)"
  )
})



test_that("full_argument_names works", {
  all_args <- c("", "pattern", "replacement", "x")
  
  expect_equal(
    object = full_argument_names(parse(text = "gsub(' ', '_', 'a b c')")[[1]]),
    expected = all_args
  )
  
  expect_equal(
    object = full_argument_names(expression(gsub(x = "a b c", " ", "_"))[[1]]),
    expected = all_args[c(1,4,2,3)]
  )
  
  expect_equal(
    object = full_argument_names(expression(gsub(x = "a b c", pat = " ", rep = "_"))[[1]]),
    expected = all_args[c(1,4,2,3)]
  )
})

