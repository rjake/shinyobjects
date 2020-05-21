library(mockery)

mock_text <- list(selection = list(x = list(text = "x <- reactive(1+1)")))

test_that("convert_selection uses provided environment - R", {
  e <- new.env()
  stub(convert_selection, "getSourceEditorContext", mock_text)
  convert_selection(envir = e)
  expect_true(class(e$x) == "function")
})


# test_that("convert_selection uses global environment", {
#   stub(convert_selection, "getSourceEditorContext", mock_text)
#   stub(ask_for_environment, "menu", 1, 2)
#   convert_selection()
#   expect_true(length(ls(.GlobalEnv)) == 1)
# })

rm(mock_text)