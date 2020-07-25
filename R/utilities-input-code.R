
#' Look for input <- demo
#'
#' @param file to evaluate
#'
#' @importFrom readr read_file
#' @importFrom stringr str_replace_all
#' @importFrom knitr purl
#' @noRd
#' @examples 
#' if (interactive()) {
#' find_input_code("inst/shiny/server.R")
#' find_input_code("inst/Rmd/flexdashboard_demo.Rmd")
#' }
find_input_code <- function(file){
  # if an R file just parse
  if (grepl("\\.R$", file, ignore.case = TRUE)) {
    parsed <- parse(file)
    
  } else {# if an Rmd, convert eval = F statement to T to see if "input <-" exists
    replace_evals <-
      read_file(file) %>%
      str_replace_all("eval( )?=( )?F(ALSE)?", "eval = TRUE")
    
    output = tempfile()
    # create R doc from Rmd
    knitr::purl(text = replace_evals, output = output, quiet = TRUE)
    
    parsed <- parse(output)
  }
  
  # R files should use "dummy_input <-", Rmd should use "input <-"
  input_code <- parsed[grepl("^(dummy_)?input(\\s)?(<-|=[^=])", parsed)]
  
  ifelse(
    length(input_code) > 0,
    as.character(gsub("dummy_", "", input_code)),
    ""
  )
}



#' Look for input usage
#'
#' @param file file to evaluate
#'
#' @description Prints a statement about the inputs that are either listed or missing
#' @importFrom stringr str_extract_all str_remove
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number filter distinct group_by summarise n ungroup
#' @importFrom tidyr unnest
#' @importFrom glue glue glue_collapse
#' @noRd
#' @examples
#' if (interactive()) {
#' input_usage(file = "inst/shiny/server.R")
#' input_usage(file = "inst/Rmd/flexdashboard_demo.Rmd")
#' }
#' 
input_usage <- function(file) {
  df <-
    tibble(text = trimws(read_lines(file = file))) %>%
    mutate(
      line = row_number(),
      text = str_remove(.data$text, "#.*") # remove comments
    ) %>%
    filter(str_detect(.data$text, "input\\$[\\w\\._0-9]+"))

  if (nrow(df) > 0) {
    df <-
      df %>%
      mutate(input_name = str_extract_all(.data$text, "input\\$[\\w\\._0-9]+")) %>%
      unnest(.data$input_name) %>%
      distinct(.data$input_name, .data$line) %>%
      group_by(input_name = str_remove(.data$input_name, "input\\$")) %>%
      summarise(
        times_used = n(),
        lines = glue_collapse(.data$line, sep = ", ")
      ) %>%
      ungroup()
  }

  df
}



#' Validate demo input statement
#'
#' @param file file to evaluate
#'
#' @description Prints a statement about the inputs that are either listed or missing
#' @importFrom stringr str_extract_all str_remove
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number filter distinct group_by summarise n select arrange
#' @importFrom tidyr unnest
#' @importFrom pander pandoc.table
#' @importFrom glue glue glue_collapse
#' @importFrom styler style_text
#' @noRd
#' @examples 
#' if (interactive()) {
#' validate_inputs("inst/Rmd/test_dashboard_missing_inputs.Rmd")
#' validate_inputs("inst/Rmd/test_dashboard_no_inputs.Rmd")
#' validate_inputs("inst/shiny/server.R")
#' }
validate_inputs <- function(file) {
  input_code <- find_input_code(file)
  input_use <- input_usage(file)
  
  if (nrow(input_use) > 0) {
    input_demo_values <-
      input_code %>%
      str_extract_all("([\\w\\.\\_0:9]+)(?=\\s\\=)") %>%
      unlist()
    
    input_ref <-
      input_usage(file) %>%
      mutate(
        missing = (!.data$input_name %in% input_demo_values | length(input_demo_values) == 0),
        status = ifelse(missing, "missing", "have")
      )
    
    
    if (nrow(input_ref) > 0 & sum(input_ref$missing) > 0) { # missing references
      message("Here are the inputs you have listed:\n")
      input_ref %>%
        select(.data$status, input = .data$input_name, .data$lines) %>%
        arrange(.data$status) %>%
        pander::pandoc.table(justify = "left", split.cells = 25)
      
      input_df <-
        input_ref %>%
        filter(missing == TRUE)
      
      input_add <-
        glue('{input_df$input_name} = ""') %>%
        glue_collapse(sep = ", \n")
      
      is_rmd <- str_detect(file, "[rR]md$")
      
      if (input_code == "") { # no input demo, create new list
        update_input_code <- glue("input <- list({input_add})")
        
        if (is_rmd) {
          message("\n# Add this code chunk to your Rmd:\n")
          message("```{r input_demo, eval = FALSE}")
          print(styler::style_text(update_input_code))
          message("```")
        } else {# is R file
          message("\n# Add this code to your R file:\n")
          print(styler::style_text(glue("dummy_{update_input_code}")))
        }
      } else { # append list
        message("Update code:")
        update_input_code <- glue("input <- list(\n..., \n{input_add}\n)")
        # str_replace(trimws(input_demo), "\\)$", glue("\n, {input_add})"))
        print(styler::style_text(update_input_code))
        
      }
    }
  }
  input_code
}



