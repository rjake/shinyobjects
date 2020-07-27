#' tryCatch eval
#'
#' @noRd
#'
eval_code <- function(x, envir = NULL) {
  as_char_x <- as.character(x)

  tryCatch(
    eval(x, envir = envir),
    error = function(e) {
      message("there was an error")
      print(glue::glue(as_char_x))
    },
    warning = function(w) {
      message("there was a warning")
      print(glue::glue(as_char_x))
    }
  )
}


#' Confirm that function is shiny version of function
#' See tests
#' @noRd
#' @importFrom rlang eval_bare
#' 
confirm_function <- function(expr, fun) {
  identical(eval_bare(expr), fun)
}



#' Find expression one level below current call
#'
#' @param x 
#' @param name 
#' @noRd
#'
#' @examples
#' code <- expression(y <- eventReactive(input$button, {print(input$n)}))
#' return_inner_expression(x = code[[1]][[3]], name = "valueExpr")
return_inner_expression <- function(x, name) {
  i <- which(full_argument_names(x) == name)
  x[[i]]
}


#' Get name of assigned  function from expression
#'
#' @param x 
#' @noRd
#' @examples
#' full_argument_names(expression(gsub(' ', '_', 'a b c')))
#' full_argument_names(expression(gsub(x = 'a b c', ' ', '_')))
#' full_argument_names(expression(gsub(x = 'a b c', pat = ' ', rep = '_')))
full_argument_names <- function(x) {
  x_fn <- x
  
  default_args <- c("", formalArgs(args(eval(x_fn[[1]]))))
  missing_names <- is.null(names(x_fn))
  
  seq_args <- seq_along(x_fn)
  #skip_last <- head(seq_args, -1)
  
  
  if (missing_names) {
    # assign names after first position
    names(x_fn) <- default_args[seq_args]
  } else {
    
    orig_args <- names(x_fn)
    has_name <- nchar(orig_args) > 0
    
    # line up args including partial matches
    explicit_args <- pmatch(orig_args[has_name], default_args)
    # update names
    names(x_fn)[which(has_name)] <- default_args[explicit_args]
    updated_args <- names(x_fn)
    
    # missing args
    avail_args <- setdiff(default_args, updated_args[has_name])
    missing_name <- which(!has_name)
    implicit_args <- avail_args[seq_along(missing_name)]
    # update names
    names(x_fn)[missing_name] <- implicit_args
  }
  
  names(x_fn)
}
