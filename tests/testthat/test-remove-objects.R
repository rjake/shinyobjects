library(mockery)

e <- new.env()

items <-
  list(
    df = iris,
    df2 = iris,
    x = runif(10)
  )


test_that("remove_objects() keeps objects", {

  stub(remove_objects, "menu", 1)

  # only keep items starting with "df"
  remove_objects(keep = "^df", envir = list2env(items, e))
  expect_equal(
    object = ls(e),
    expected = c("df", "df2")
  )

  # confirm output lists items being removed
  x <-
    capture_message(
      remove_objects(
        keep = "^df",
        envir = list2env(items, envir = e)
      )
    )

  expect_true(grepl("these items will be removed.*- x", x))
  # then drop everything
  list2env(items, envir = e)
  remove_objects(envir = e)
  expect_equal(
    object = length(ls(e)),
    expected = 0
  )
  
  # no more items
  expect_message(remove_objects(envir = e), "No items to remove")
})


test_that("messages when remove_objects() is canceled", {
  stub(remove_objects, "menu", 2)

  keep_none <- remove_objects(envir = list2env(items, envir = e))
  expect_true(grepl("Please specify", keep_none))
  
  keep_some <- remove_objects(keep = "x", envir = list2env(items, envir = e))
  expect_true(grepl("Please update", keep_some))
})
