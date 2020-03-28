library(mockery)

test_that("file name used", {
  expected <- "test.R"
  actual <- which_file(expected)
  expect_equal(actual, expected)
})


test_that("current source editor used", {
  expected <- "test.R"
  stub(
    which_file,
    "rstudioapi::getSourceEditorContext",
    list(path = expected)
  )
  stub(which_file, "is.null", FALSE)
  stub(which_file, "menu", 1)
  
  actual <- which_file()
  
  expect_equal(actual, expected)
})


test_that("file.choose used bc missing source context", {
  expected <- "test.R"
  file_choose_mock <- mock()
  stub(
    which_file,
    "rstudioapi::getSourceEditorContext",
    NULL
  )
  stub(which_file, "file.choose", expected) #file_choose_mock)
  actual <- which_file()
  expect_equal(actual, expected)
  # which_file()
  # expect_called(file_choose_mock, n = 1)
})


test_that("file.choose used because selected in menu", {
  expected <- "test.R"
  stub(
    which_file,
    "rstudioapi::getSourceEditorContext",
    list(path = expected)
  )
  stub(which_file, "is.null", FALSE)
  stub(which_file, "menu", 2)
  stub(which_file, "file.choose", expected, depth = 2)
  actual <- which_file()
  expect_equal(actual, expected)
})

