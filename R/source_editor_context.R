# nocov start

#' Convert and load the hightlighted assignment to the global environment
#'
#' After highlighting the assignment in the source editor, go to 
#' the console and run this function. The selected code will be run 
#' and if it is reactive, it will be loaded as a funciton.
#'  
#' @export
#' 
#' @importFrom rstudioapi getSourceEditorContext
#'
convert_selection <- function() {
  orig_code <- getSourceEditorContext()$selection[[1]]$text
  new_code <- convert_assignments(orig_code)
  eval(parse(text = new_code) , envir = .GlobalEnv)
}

# nocov end