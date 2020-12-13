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
#' @importFrom rlang parse_exprs
#' @noRd
find_all_assignments_rmd <- function(file) {
  tmp <- purl(file, output = tempfile(), quiet = TRUE)
  x <- parse_exprs(file(tmp))
  find_all_assignments_r(x)
}


# Convert ----

# see notes from Garrick Aden-Buie
# https://gist.github.com/gadenbuie/cc386bdc6a636ba592c520d96af82e3f

#' Update expressions to be non-reactive
#' @param x code to evaluate
#' @noRd
#' @importFrom rlang expr call_standardise
#' @examples
#' update_expressions(
#'  x = expr(y <- eventReactive(input$button, {print(input$n)}))
#' )
#' update_expressions(
#'  x = expr(output$plot <- renderPlot(plot(1, 1)))
#' )
#' update_expressions(
#'  x = expr(output$plot <- shiny::renderPlot(plot(1, 1)))
#' )
update_expressions <- function(x){
  #char_code <- as.character(as.expression(x))
  # code_as_call <- as.call(x)
  
  # withProgress(...) -> (...) ----
  # not usually assigned
  if (confirm_function(x[[1]], shiny::withProgress)) {
    new_expr <- expr(!!call_standardise(x)[["expr"]])
    
    return(new_expr)
  }
  
  # exceptions ----
  # if not assigned (ex: library(...))
  if (
    x[[1]] != as.symbol("<-") & 
    x[[1]] != as.symbol("=") &
    length(x) != 3
  ) {
    return(x)
  }
  
  # if no function involved
  if (!is.language(x[[3]])) {
    return(x)
  }
  
  # extract parts ----
  get_symbol   <- x[[2]]
  get_identity <- x[[3]]
  get_fn       <- get_identity[[1]]
   
  # if assignment != symbol/function return [[1]] else [[2]]
  if (length(get_identity) == 1) {
    get_formals  <- get_identity[[1]]      
  } else {
    get_formals  <- get_identity[[2]]
  }

  # reactive(...) -> function() {...} ----
  if (confirm_function(get_fn, shiny::reactive)) {
    new_expr <- expr(!!get_symbol <- function() { 
      !!get_formals 
    })
    
    return(new_expr)
  }
  
  # eventReactive(...) -> function() {...} ----
  if (confirm_function(get_fn, shiny::eventReactive)) {
    new_expr <- expr(!!get_symbol <- function() {
      !!call_standardise(get_identity)[["valueExpr"]]
    })
    
    return(new_expr)
  }
  
  # reactiveValues(...) -> list(...) ----
  if (confirm_function(get_fn, shiny::reactiveValues)) {
    x[[3]][[1]] <- as.symbol("list")
    return(x)
  }
  
  # reactiveVal(...) -> list(...) ----
  if (confirm_function(get_fn, shiny::reactiveVal)) {
    use_symbol <- as.character(get_symbol)
    new_expr <-
      expr(
        !!get_symbol <- function(value, label = "") {
          if (!missing(value)) {
            .shinyobjects_reactiveVal[[!!use_symbol]] <<- value
          } else {
            .shinyobjects_reactiveVal[[!!use_symbol]]
          }
        }
      )
    return(new_expr)
  }
  
  # reactiveValuesToList(...) -> list(...) ----
  if (confirm_function(get_fn, shiny::reactiveValuesToList)) {
    x[[3]][[1]] <- as.symbol("as.list")
    return(x)
  }

  # if not an x$y or x[[y]] object ----
  if (length(get_symbol) == 1) {
    return(x)
  }
  
  # if not output$x ----
  if (get_symbol[[2]] != as.symbol("output")) {
    return(x)
  }
  
  # renderPlot(...) -> recordPlot(...) ----
  if (confirm_function(get_fn, shiny::renderPlot)) {
      new_exp <- expr(!!get_symbol <- grDevices::recordPlot(!!get_formals))
      
      return(new_exp)
  } 
  
  new_exp <- expr(!!get_symbol <- !!get_formals)
  
  return(new_exp)
}


#' Convert reactive dataframes to functions
#'
#' @param x text to be converted
#' @importFrom rlang exprs
#' @noRd
#' @examples 
#' convert_assignments(
#'   x = exprs(a <- reactive(123), output$x <- renderTable(mtcars))
#' )
convert_assignments <- function(x) {
  
  exp_list <- exprs()
  
  for (i in seq_along(x)) {
    new_code <-
      tryCatch(
        update_expressions(x[[i]]),
        error = function(e) {
          message("there was an error")
          print(glue::glue(as.character(x)))
        }
      )
    
    exp_list <- 
      append(
        exp_list, 
        new_code,
        after = i - 1
      )
  }
  
  exp_list
}
