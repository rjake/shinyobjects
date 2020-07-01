#' Find all libraries and assignments for R files
#'
#' @param x code to evaluate
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom stringr str_detect
#' @noRd
find_all_assignments_r <- function(x) {
  code <- as.character(parse(text = x))
  code[str_detect(code, strings_to_find())]
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
  x <- as.character(parse(tmp)) %>% trimws()
  find_all_assignments_r(x)
}


#' Convert reactive dataframes to functions
#'
#' @param x text to be converted
#' @importFrom stringr str_detect str_replace_all
#' @noRd
convert_assignments <- function(x){
  x %>%
    str_replace_all("\\b(shiny::)?reactive\\(", "function() (") %>%
    str_replace_all("\\b(shiny::)?reactiveValues\\(", "list(")
}



#' Convert R code to a data frame
#'
#' @param code to evaluate
#'
#' @importFrom tibble tibble
#' @importFrom dplyr rowwise mutate ungroup
#' @noRd
code_to_df <- function(code) {
  tibble(raw = as.character(code)) %>%
    rowwise() %>%
    mutate(code = convert_assignments(raw)) %>%
    ungroup()
}
