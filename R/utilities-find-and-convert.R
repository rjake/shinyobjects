#' Find all libraries and assignments for R files
#'
#' @param x code to evaluate
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom stringr str_detect
#' @noRd
find_all_assignments_r <- function(x) {
  x[str_detect(as.character(x), strings_to_find())]
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


#' Update expressions to be non-reactive
#' @param x code to evaluate
#' @noRd
update_expressions <- function(x){
  if (grepl("reactive\\(", as.character(x))) {
    code_as_call <- as.call(x)[[1]] 
    get_symbol <- code_as_call[[2]] 
    get_formals <- code_as_call[[3]][[2]]
    new_exp <-
      as.expression(
        bquote(
          .(get_symbol) <- function() .(get_formals)
        )
      )
    
    final_code <- new_exp
  } else if (grepl("reactiveValues\\(", as.character(x))) {
    code_as_call <- as.call(x)[[1]] 
    code_as_call[[3]][[1]] <- as.symbol("list")
    
    final_code <- as.expression(code_as_call)
  } else {
    final_code <- x
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
