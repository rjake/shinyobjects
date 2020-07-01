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



#' Returns server code from shinyApp or runApp
#' 
#' @param text code stored as text
#' @noRd
extract_from_app_fn <- function(text) {
  
  code <- gsub("shiny::", "", text)
  
  if (!grepl("server", code)) {
    message("server not listed, using whole file")
    inside_code <- ""
  } else if (grepl("^runApp.*server =", code)) {
    inside_code <- inside_runapp(code)
  } else if (grepl("shinyApp.*server =", code)) {
    inside_code <- inside_shinyapp(code)
  } 
  
  deparse_server(inside_code)
}



#' Pulls the calls out of runApp
#' 
#' @param x expression containing runApp(...) 
#' @noRd
#' @importFrom purrr pluck
inside_runapp <- function(code) {
  sub("runApp\\(shinyApp", "runApp(list", code) %>%
    guts("runApp") %>%
    guts("list") %>%
    pluck("server") %>%
    paste(collapse = "\n") %>%
    gsub(pattern = "^[^\\{]+\\{", replacement = "") %>% 
    gsub(pattern = "\\}[^\\}]+$", replacement = "")
}


#' Pulls the calls out of shinyApp
#' 
#' @param x expression containing runApp(...) 
#' @noRd
inside_shinyapp <- function(x){
  guts(x, "shinyApp") %>% 
    pluck("server")
}



#' Returns asignments only from expressions
#' 
#' @param x expression from code stored as text
#' @noRd
deparse_server <- function(x) {
  parse(text = x) %>% 
    trimws() %>% 
    find_all_assignments_r()
}



#' Returns what was called inside of a function stored as text
#' 
#' @param x the code stored as text
#' @param fn the function to sub out
#' @noRd
#' @examples
#' if (interactive()) {
#' x <- "mean(list(1:10, 10:30))"
#' guts(x, "mean")
#' guts(x, "mean") %>% guts("list")
#' }
guts <- function(x, fn) {
  return_args <- function(...) {
    (as.list(match.call(expand.dots = T)))
  }
  
  assign(fn, return_args)
  
  eval(parse(text = x))
}
