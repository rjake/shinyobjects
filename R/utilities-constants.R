#' Valid strings for assignments/column names
#' @noRd
strings_to_find <- function() {
  paste0(
    "^(library|", 
    "[\\w\\.\\$0:9]+", 
    " (<-|=[^=]))"
  )
}
