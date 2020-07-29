library(mockery)

mock_text <- list(
  selection = list(
    x = list(
      text = "x <- reactive(1+1)\n
      y <- reactiveValues(a = 1, b = 2)"
    )
  )
)

test_that("convert_selection uses provided environment - R", {
  e <- new.env()
  stub(convert_selection, "getSourceEditorContext", mock_text)
  convert_selection(envir = e)
  expect_true(class(e$x) == "function")
})

rm(mock_text)
