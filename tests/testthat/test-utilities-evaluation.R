suppressWarnings(library(rlang))
suppressWarnings(library(shiny))

# eval_code() ----
test_that("eval_code() works", {
  x <- eval_code(expr(1 + 1))
  
  expect_equal(x, 2)
  
  expect_message(eval_code(expr(stop())), "there was an error")
  expect_message(eval_code(expr(warning())), "there was a warning")
})



# confirm_function() ----
test_that("confirm_function works", {
  code <-
    rlang::parse_exprs(
      "n_obs <- reactive(nrow(df()))
      
      df = shiny::reactive({
        x <- input$cty
        mpg %>% filter(cty < x)
      })
    
      this_is_crazy <- shiny::reactive
      #df_head <- this_is_crazy(head(df()))
      
      runApp(list(ui = NULL, server = function(input, output) {NULL}))
      shiny::runApp(list(ui = NULL, server = function(input, output) {NULL}))
      
      shinyApp(ui = NULL, server = function(input, output) {NULL})
      "
    )
  
  expect_true(confirm_function(code[[1]][[3]][[1]], shiny::reactive))
  expect_true(confirm_function(code[[2]][[3]][[1]], shiny::reactive))
  expect_true(confirm_function(code[[3]][[3]][[3]], shiny::reactive))
  expect_true(confirm_function(code[[4]][[1]], shiny::runApp))
  expect_true(confirm_function(code[[5]][[1]], shiny::runApp))
  expect_true(confirm_function(code[[6]][[1]], shiny::shinyApp))
  
  # e <- new.env()
  # for (expr in code) eval(expr, e)
  #confirm_function(code[[4]][[3]][[1]], shiny::reactive)
})



# full_argument_names() ----

