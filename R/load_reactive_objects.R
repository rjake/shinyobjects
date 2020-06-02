#' Load inputs and convert reactive functions from an R/Rmd script to your environment
#'
#' @description This function will run all assignments of your R or Rmd. file In the process, this function will encourage the creation of a dummy \code{input} list that will mimic user input and allow your code to run. Lastly, reactive objects are converted to functions so they can still be called as \code{df()} etc.
#' @section Warning:
#' This function has the ability to overwrite your objects in your environment. Make sure you understand how this function works before moving forward.
#'
#' @param file Rmd to be evaluated and loaded into your environment
#' @param clear_environment When \code{TRUE}, will remove objects not named in \code{...}
#' @param restart When \code{TRUE}, will restart the current R session. If you have R default to restore RData by default, you will need to use the \code{clear_environment} argument as well
#' @param keep a regular expression of objects to keep when \code{clear_environment = TRUE}
#' @param envir the environment shinyobjects should the load the objects into.
#'
#' @export
#' @importFrom readr read_lines
#' @importFrom rstudioapi restartSession
#'
#' @examples
#' if (interactive()) {
#'   system.file(package = "shinyobjects", "Rmd/test_dashboard.Rmd") %>%
#'     load_reactive_objects()
#'
#'   system.file(package = "shinyobjects", "Rmd/test_dashboard_no_inputs.Rmd") %>%
#'     load_reactive_objects()
#'
#'   system.file(package = "shinyobjects", "Rmd/test_dashboard_missing_inputs.Rmd") %>%
#'     load_reactive_objects()
#' }
load_reactive_objects <- function(file,
                                  restart = FALSE,
                                  envir = NULL,
                                  clear_environment = FALSE,
                                  keep = NULL) {
  # nocov start
  stopifnot(interactive())
  # nocov end
  
  # confirm environment
  if (missing(envir)) {
    envir <- ask_for_environment()
  }
  
  # select file if not provided
  file_to_parse <- which_file(file)

  # check if Rmd or R
  is_rmd <- str_detect(file_to_parse, "[rR]md$")

  # make sure demo inputs exist (if required)
  validate_inputs(file_to_parse)

  # nocov start
  if (restart) {
    rstudioapi::restartSession()
  }

  if (clear_environment) {
    # remove_object will return "cleared" if successful
    result <- remove_objects(keep, envir = envir)
    if (result != "cleared") {
      stop(result, call. = FALSE)
    }
  } else {
    result <- "proceed"
  }
  # nocov end

  if (result %in% c("cleared", "proceed")) {
    # * load inputs ----
    eval(parse(text = find_input_code(file_to_parse)), envir = envir)

    # find all libraries and functions ----

    if (is_rmd) {
      # code as tibble (orig + converted functions)
      final_code <-
        find_all_assignments_rmd(file_to_parse)
    } else {
      # parsed code
      final_code <-
        breakout_server_code(file_to_parse) %>%
        find_all_assignments_r()
    }

    # parsed code
    text_to_parse <-
      code_to_df(final_code)$code

    # add library(shiny) if not included
    if (max(grepl("library\\(shiny\\)", text_to_parse)) == 0) {
      text_to_parse <-
        c("library(shiny)", text_to_parse)
    }

    # list of expressions
    parsed_code <- parse(text = text_to_parse)

    # create ouput & session lists so assignments don't break
    assign("output", list(), envir)
    assign("session", list(), envir)

    # final evaluation
    for (i in seq_along(parsed_code)) {
      eval_code(parsed_code[i], envir = envir)
    }
  }
}
