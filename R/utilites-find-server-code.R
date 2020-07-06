#' Parse server file for assignments & inputst
#'
#' @param file file to parse
#' @noRd
#' @examples 
#' if (interactive()) {
#' breakout_server_code(file = "inst/shiny/server.R") %>% 
#' substr(1, 30)
#' }
breakout_server_code <- function(file) {
  raw_code <- as.character(parse(file))

  server_line <- grep("^(shiny)?server\\b", raw_code, ignore.case = TRUE)
  app_line <- grep("^(shiny|run)App\\(.*server =", raw_code, ignore.case = TRUE)
  has_server_line <- length(server_line) > 0
  has_app_line <- length(app_line) > 0

  if (length(server_line) > 1) {
    stop("more than one server assignment found", call. = FALSE)
  }

  if (!has_server_line & !has_app_line) { # treat all code as "server" code
    final_code <- raw_code
  } else {
    server_code <- raw_code[server_line]

    if (has_app_line) {
      # extract out of "runApp()" or "shinyApp()"
      server_code <- raw_code[app_line]
      new_code <- extract_from_app_fn(server_code)
      final_code <- append(raw_code, new_code, after = app_line)[-app_line]
    } else if (has_server_line) {
      # convert the "server <-" line
      server_code <- raw_code[server_line]
      new_code <- code_between(server_code, pattern = "c")
      final_code <- append(raw_code, new_code, after = server_line)[-server_line]
    }
  }

  unlist(lapply(final_code, convert_assignments))
}



