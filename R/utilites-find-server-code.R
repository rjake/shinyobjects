# Main function ----

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
  # file <- "tests/testthat/demo-r-runapp-shinyapp_assigned.R"
  
  code <- parse(file)
  char_code <- as.character(code)

  if (!any(grep("server.*(=|<-)", char_code))) {
    code
  
  } else if (any(server_is_assigned(char_code))) {
    extract_from_server_assignment(code)
    
  } else if (is_server_file(file)) {
    extract_from_server_file(code)
    
  } else {
   extract_from_app(code)
  }
}


# Extract ----
extract_from_server_assignment <- function(code) {
  server_line <- which(server_is_assigned(code))
  
  if (length(server_line) > 1) {
    server_line <- server_line[1]
    warning(
      "'server' was assigned twice, only the first instance will be used",
      call. = FALSE
    )
  }
  
  server_code <- code[server_line][[1]][[3]]
  
  if (length(server_code) >= 3) { # confirm it is a function
  
    # alternative: return(body(eval(code[server_line]))[-1])
    update_code(
      code = code,
      server_code = server_code[[3]][-1], # -1 removes "{" from expression
      replace_line = server_line
    )
  }
}


extract_from_server_file <- function(code) {
  char_code <- code
  
  if (!any(server_is_assigned(char_code))) {
    code
  } else {
    extract_from_server_assignment(code)
  }
}


extract_from_app <- function(code) {
  # expressions are essentially lists, you can use View() to explore
  # ...[-1] removes "{" from expression structure
  
  # fild line that has server, has to have '...App(..., server = ...)'
  app_line <- 
    which(grepl(
      pattern = "(run|shiny)App\\(.*server(\\s)?=", 
      x = as.character(code)
    ))
  
  orig_code <- code[app_line]
  
  # confirm type
  is_assigned <- as.character(orig_code[[1]][[1]]) %in% c("=", "<-")
  is_shinyapp <- confirm_function(orig_code[[1]][[1]], shiny::shinyApp)
  is_runapp <- confirm_function(orig_code[[1]][[1]], shiny::runApp)

  if (is_assigned) {
    server_code <- orig_code[[1]][[3]][["server"]][[3]][-1]
  } else if (is_shinyapp) {
    server_code <- orig_code[[1]][[2]][[3]]
  } else if (is_runapp) {
    server_code <- orig_code[[1]][[2]][["server"]][[3]][-1]
  }
  
  update_code( 
    code = code,
    server_code = server_code,
    replace_line = app_line
  )
}


# Update code ----
update_code <- function(code, server_code, replace_line){
  append(
    x = code, 
    values = as.list(server_code), 
    after = replace_line
  )[-replace_line]
}


# Test T / F ----
# Find location of server logic

is_server_file <- function(file) {
  grepl(
    pattern = "server.r", 
    x = file, 
    ignore.case = TRUE
  )
}


server_is_assigned <- function(code) {
  grepl(
    pattern = "^(shiny::)?(shiny)?server(\\s)*(=|<-)", 
    x = as.character(code), 
    ignore.case = TRUE
  )
}
