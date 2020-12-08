#' Convert and load the highlighted assignment to your environment
#'
#' After highlighting the assignment in the source editor, go to 
#' the console and run this function. The selected code will be run 
#' and if it is reactive, it will be loaded as a function.
#' @param envir the environment shinyobjects should the load the objects into.
#' @importFrom rlang parse_exprs
#' @export
#' 
#' @importFrom rstudioapi getSourceEditorContext
#'
convert_selection <- function(envir = NULL) {
  if (missing(envir)) {
    envir <- ask_for_environment()
  }
  
  orig_code <- getSourceEditorContext()$selection[[1]]$text
  new_code <- convert_assignments(parse_exprs(orig_code))
  
  for (i in seq_along(new_code)) {
    eval(new_code[[i]], envir = envir)
  }
}

