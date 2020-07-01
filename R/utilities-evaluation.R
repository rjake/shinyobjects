#' tryCatch eval
#'
#' @noRd
#'
eval_code <- function(x, envir = NULL) {
  as_char_x <- as.character(x)

  tryCatch(
    eval(x, envir = envir),
    error = function(e) {
      message("there was an error")
      print(glue::glue(as_char_x))
    },
    warning = function(w) {
      message("there was a warning")
      print(glue::glue(as_char_x))
    }
  )
}
