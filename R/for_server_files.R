#' Find text between brackets or parentheses
#'
#' @param text the text to scan
#' @param pattern look between "c" curly braces or "p" parenthese 
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_locate_all
#' @noRd
#' @examples
#'  \dontrun{
#' "here is text (between two parentheses), as an example" %>% 
#'   char_between("p")
#'   
#' "here is text {between two curly braces}, as an example" %>% 
#'   char_between("c")
#' }
char_between <- function(text, pattern = c("c", "p")) {
  if (missing(pattern)) {
    warning(
      'pattern not specified, curly braces {} used for parsing', 
      call. = FALSE
    ) 
  }
  
  pattern <- match.arg(pattern)
  regex_pattern <-
    ifelse(
      pattern == "c", 
      "[\\{\\}]",
      "[\\(\\)]" 
    )
  
  pattern_match <-  
    str_locate_all(string = as.character(text), pattern = regex_pattern)[[1]] %>% 
    range()
  
  substr(text, pattern_match[1] + 1, pattern_match[2] - 1) %>% 
    trimws()
  
}



#' Parse server file for assignments & inputst
#'
#' @param file file to parse
#' @noRd
#' @examples 
#' \dontrun{
#' breakout_server_code(file = "inst/shiny/server.R") %>% 
#'   eval(envir = .GlobalEnv)
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
    if (has_app_line) { # extract out of "runApp()" or "shinyApp()"
      server_code <- raw_code[app_line]
      new_code <- extract_from_app_fn(server_code)
      final_code <- append(raw_code, new_code, after = app_line)[-app_line]
    } else if (has_server_line) { # convert the "server <-" line
      server_code <- raw_code[server_line]
      new_code <- char_between(server_code, pattern = "c")
      final_code <- append(raw_code, new_code, after = server_line)[-server_line]
    }
    
  }
  
  unlist(lapply(final_code, convert_assignments))
}
