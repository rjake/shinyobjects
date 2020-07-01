#' Select file to use
#'
#' @param file path to file. 
#'
#' @description If the file is not specified, a menu will appear asking the 
#' user if they want to use the active source file loaded in RStudio or if they 
#' want to select the file (opens a new window).
#' @importFrom utils menu
#' @noRd
which_file <- function(file) {
  if (!missing(file)) {
    file_to_parse <- file
  } else {
    current_source <- rstudioapi::getSourceEditorContext()$path
    if (is.null(current_source)) {
      file_to_parse <- file.choose()
    } else {
      current_text <- basename(current_source)
      find_file <-
        menu(c(
          paste("Use current file:", current_text),
          "Choose file in browser"
        ))
      if (find_file == 1) {
        file_to_parse <- current_source
      } else {
        file_to_parse <- file.choose()
      }
    }
  }
  
  file_to_parse
}


#' Ask user which environment to use
#'
#' @importFrom utils menu
#' @noRd
ask_for_environment <- function() {
  res <-
    menu(
      choices = c("Global", "New", "Cancel"),
      title = "WARNING: Which environment do you want to use?"
    )
  
  switch(
    res,
    "1" = .GlobalEnv,
    "2" = new.env(),
    "3" = stop("Canceled", call. = FALSE)
  )
}