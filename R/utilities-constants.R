#' Valid strings for assignments/column names
#' @noRd
strings_to_find <- function() {
  paste0(
    "^((library|require)\\(|", 
    "[\\w\\._\\$0:9]+", 
    "(\\s)?(<-|=[^=]))"
  )
}

#' list to hold values for reactiveVal objects
#' modifies in place
#' @noRd
.shinyobjects_reactiveVal <- list()
