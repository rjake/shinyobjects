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
  
  # see if script has server <- ... or shinyServer <- ...
  server_line <-
    which(grepl("^(shiny::)?(shiny)?server\\b", char_code, ignore.case = TRUE))
  
  starts_server <- any(server_line > 0)
  
  app_line <- which(grepl("(run|shiny)App\\(.*server(\\s)?=", char_code))
  has_app <- any(app_line > 0)
  
  # expressions are essentially lists, you can use View() to explore
  if (!starts_server && !has_app) {
    final_code <- code
  } else {
    
    if (starts_server) {# script is only server <- ....
      server_code <- code[server_line][[1]][[3]][[3]]
      replace_line <- server_line
    } else {# has shinyApp or runApp
      
      if (has_app) {
        
        orig_code <- code[app_line]
        replace_line <- app_line
        
        is_assigned <- as.character(as.list(orig_code)[[1]][[1]]) %in% c("=", "<-")
        is_runapp <- grepl("runApp$", as.list(orig_code)[[1]][[1]])
        
        if (is_assigned) {
          app_code <- orig_code[[1]][[3]]
          server_code <- as.list(app_code[[3]][[3]][-1])
        } else if (is_runapp) {
          server_code <- orig_code[[1]][[2]][[3]][[3]]
        }
      }
    }
    
    final_code <- 
      append(code, as.list(server_code), after = replace_line)[-replace_line]
  }
  
  final_code
}
