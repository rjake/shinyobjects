#' Find code between brackets: (), {}, []
#' code_between("test {x {y}}", "c")
#' code_between("test {x (y)}", "p")
#' @noRd
#' @importFrom stringr str_remove str_extract
#' @importFrom purrr pluck
code_between <- function(x, pattern = "p") {
  
  use_pair <- ifelse(pattern == "p", "()", "{}")

  start <- paste0("\\", substring(use_pair, 1, 1))
  end <- paste0("\\", substring(use_pair, 2, 2))
  
  regex_pattern <-
    paste0(
      start,
      "(?>[^",
      start,
      end,
      "]|(?R))*",
      end
    )
  
  x_match <- 
    gregexpr(pattern = regex_pattern, x, perl = TRUE) %>% 
    pluck(1)
  
  substring(
    x, 
    x_match + 1, 
    x_match + attr(x_match, "match.length") - 2
  )
}

