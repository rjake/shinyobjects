#' Valid strings for assignments/column names
#' @noRd
strings_to_find <- function() {
  paste0(
    "^((library|require)\\(|", 
    "[\\w\\._\\$0:9]+", 
    "(\\s)?(<-|=[^=]))"
  )
}
