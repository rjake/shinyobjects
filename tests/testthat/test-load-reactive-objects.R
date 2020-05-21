library(mockery)

test_that("uses provided environment - R", {
  e <- new.env()
  stub(load_reactive_objects, "interactive", TRUE)
  load_reactive_objects(file = "demo-r-runapp-full.R", envir = e)
  expect_true(length(ls(e)) == 5)
})


test_that("uses provided environment - Rmd", {
  e <- new.env()
  stub(load_reactive_objects, "interactive", TRUE)
  load_reactive_objects(file = "demo-rmd-full.Rmd", envir = e)
  expect_true(length(ls(e)) == 5)
})



# test_that("uses global environment", {
#   stub(load_reactive_objects, "interactive", TRUE)
#   stub(ask_for_environment, "menu", 1, 2)
#   load_reactive_objects(file = "demo-r-runapp-full.R")
#   print(length(ls(.GlobalEnv)))
#   expect_true(length(ls(.GlobalEnv)) == 6)
# })


test_that("uses selected file", {
  e <- new.env()
  stub(load_reactive_objects, "interactive", TRUE)
  stub(load_reactive_objects, "which_file", "demo-r-runapp-full.R")
  load_reactive_objects(envir = e)
  expect_true(length(ls(e)) == 5)
})


test_that("clears environment", {
  e <- test_env()
  list2env(list(df1 = iris, df2 = iris3, x = runif(10)), envir = e)
  stub(load_reactive_objects, "interactive", TRUE)
  stub(remove_objects, "menu", 1)
  load_reactive_objects(
    file = "demo-r-runapp-full.R",
    clear_environment = F,
    keep = "x",
    envir = e
  )
  expect_true(length(ls(e)) == 8)
})
