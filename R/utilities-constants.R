#' Valid strings for assignments/column names
#' @noRd
valid_object_names <- function() {
  "[\\w\\.\\$0:9]+"
}

#' Valid strings for assignments/column names
#' @noRd
strings_to_find <- function() {
  paste0("^(library|", valid_object_names(), " (<-|=[^=]))")
}
