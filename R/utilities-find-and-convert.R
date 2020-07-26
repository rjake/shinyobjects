# Find ----

#' Find all libraries and assignments for R files
#'
#' @param x code to evaluate
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom stringr str_detect
#' @noRd
find_all_assignments_r <- function(x) {
  keep_x <-
    str_detect(as.character(x), strings_to_find()) & 
    !str_detect(as.character(x), "^dummy_(input|output|session)\\b")
  
  x[keep_x]
}


#' Find all libraries and assignments for rmd
#'
#' @param file to evaluate
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom stringr str_detect
#' @noRd
find_all_assignments_rmd <- function(file) {
  tmp <- purl(file, output = tempfile(), quiet = TRUE)
  x <- parse(tmp)
  find_all_assignments_r(x)
}


# Convert ----

# see notes from Garrick Aden-Buie
# https://gist.github.com/gadenbuie/cc386bdc6a636ba592c520d96af82e3f

#' Update expressions to be non-reactive
#' @param x code to evaluate
#' @noRd
update_expressions <- function(x){
  char_code <- as.character(x)
  
  if (!grepl("<-.*\\(", char_code)) {
    final_code <- x
  } else {
    code_as_call <- as.call(x)[[1]]
    get_symbol <- code_as_call[[2]] 
    get_formals <- code_as_call[[3]][[2]]
    
    
    if (grepl("reactive\\(", char_code, ignore.case = TRUE)) {
      if (grepl("eventReactive\\(", char_code)) {
        get_formals <- code_as_call[[3]][[3]]
      } 
      new_exp <-
        as.expression(
          bquote(
            .(get_symbol) <- function() .(get_formals)
          )
        )
      
      final_code <- new_exp
    } else if (grepl("reactiveValues\\(", char_code)) {
      code_as_call[[3]][[1]] <- as.symbol("list")
      
      final_code <- as.expression(code_as_call)
    } else if (grepl("output\\$.*renderPlot", char_code)) {
      new_exp <-
        as.expression(
          bquote(
            .(get_symbol) <- recordPlot(.(get_formals))
          )
        )
      
      final_code <- new_exp
    } else if (grepl("output\\$", char_code)) {
      new_exp <-
        as.expression(
          bquote(
            .(get_symbol) <- (.(get_formals))
          )
        )
      
      final_code <- new_exp
    } else {
      final_code <- x
    } 
  }  
  final_code
}


#' Convert reactive dataframes to functions
#'
#' @param x text to be converted
#' @importFrom stringr str_detect str_replace_all
#' @noRd
convert_assignments <- function(x) {
  
  exp_list <- expression()
  
  for (i in seq_along(x)) {
    exp_list <- 
      append(
        exp_list, 
        as.list(update_expressions(x[i])),
        after = i - 1
      )
  }
  
  exp_list
}

