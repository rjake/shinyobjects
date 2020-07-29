#' Clear all objects in environment
#'
#' @param keep A regular expression of objects in environment to keep
#' 
#' @importFrom glue glue
#' @importFrom utils menu
#' @noRd
#' @examples
#' if (interactive()) {
#' e <- new.env()
#' list2env(
#'   list(
#'     df = iris,
#'     df2 = iris,
#'     x = runif(10)
#'   ),
#'   envir = e
#' )
#' remove_objects(keep = "^df", envir = e)
#' }
remove_objects <- function(keep = NULL, envir = NULL) {
  
  all_objects <- ls(envir = envir)
  base_regex <- "temp_|final_code"
  
  final_regex <- 
    ifelse(
      missing(keep), 
      base_regex, 
      paste(c(base_regex, keep), collapse = "|")
    ) %>% 
    gsub(pattern = "(\\|\\|)+", replacement = "") %>% 
    gsub(pattern = "\\|$", replacement = "") # ends with |, if keep = ""
  
  identify_objects <- !grepl(final_regex, all_objects)
  remove_objects <- all_objects[identify_objects]
  
  # list items to be removed and then remove them
  if (length(remove_objects) == 0) {
    final_result <- "No items to remove"
    message(final_result)
    
  } else {
    # list items to be removed
    message(
      paste(
        "these items will be removed or replaced when data is loaded:\n -", 
        paste(remove_objects, collapse = "\n - ")
      )
    )  
    
    # list items to be kept
    if (length(remove_objects) != length(all_objects)) {
      message(
        paste(
          "\nthese items will be kept:\n -", 
          paste(all_objects[!identify_objects], collapse = "\n - ")
        )
      )
      
      regex_phrase <- glue('update this argument: keep = "{keep}"')
    } else {
      regex_phrase <- 'specify objects using the (keep = "") argument'
    }
    
    # confirm selections
    confirm <-
      menu(
        choices = c("Looks good to me", "I need to edit this list"),
        title = "Do you want to continue? (Press 0 to exit)"
      )
    
    # clear environment if enter is used
    if (confirm == 1) {
      rm(list = remove_objects, envir = envir)
      final_result <- "cleared"
    } else {
      final_result <- glue('Please {regex_phrase}')
    }
    
    final_result
  }
}
