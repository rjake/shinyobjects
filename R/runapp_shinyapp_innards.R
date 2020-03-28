#' Returns what was called inside of a function stored as text
#' 
#' @param x the code stored as text
#' @param fn the function to sub out
#' @noRd
#' @examples
#'  \dontrun{
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


#' Returns asignments only from expressions
#' 
#' @param x expression from code stored as text
#' @noRd
deparse_server <- function(x) {
  x %>% 
    deparse() %>% 
    trimws() %>% 
    find_all_assignments_r()
}


#' Pulls the calls out of runApp
#' 
#' @param x expression containing runApp(...) 
#' @noRd
inside_runapp <- function(x){
  sub("runApp\\(shinyApp", "runApp(list", x) %>% 
    guts("runApp") %>% 
    guts("list")
}

#' Pulls the calls out of shinyApp
#' 
#' @param x expression containing runApp(...) 
#' @noRd
inside_shinyapp <- function(x){
  guts(x, "shinyApp")
}


#' Returns server code from shinyApp or runApp
#' 
#' @param text code stored as text
#' @noRd
extract_from_app_fn <- function(text) {
  
  code <- gsub("shiny::", "", text)
  
  if (!grepl("server", code)) {
    warning("server not listed", call. = FALSE)
    inside_code <- list(server = "")
  } else if (grepl("^runApp.*server =", code)) {
    inside_code <- inside_runapp(code)
  } else if (grepl("shinyApp.*server =", code)) {
    inside_code <- inside_shinyapp(code)
  } 
  
  deparse_server(inside_code$server)
}
