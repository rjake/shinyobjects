# nocov start

#' Convert and load the highlighted assignment to your environment
#'
#' After highlighting the assignment in the source editor, go to 
#' the console and run this function. The selected code will be run 
#' and if it is reactive, it will be loaded as a function.
#' @param envir the environment shinyobjects should the load the objects into.
#' @export
#' 
#' @importFrom rstudioapi getSourceEditorContext
#'
convert_selection <- function(envir = NULL) {
  orig_code <- getSourceEditorContext()$selection[[1]]$text
  new_code <- convert_assignments(orig_code)
  eval(parse(text = new_code) , envir = envir)
}

# nocov end