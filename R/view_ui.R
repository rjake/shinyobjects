#' Show UI output in viewer pane
#'
#' @param x ui content (actionButton, selectInput, valueBox), defaults to last output, expects an object with class "shiny.tag"
#' @param close_after number of seconds to display UI in Viewer panel. If NULL, app must be stopped manually before more code can be run.
#' @importFrom shiny shinyApp fluidPage runApp stopApp
#' @importFrom rstudioapi viewer
#' @export
#' @examples 
#' \dontrun{
#' # run this line
#' shiny::selectInput(
#'   "state",
#'   "Choose a state:",
#'   list(
#'     `East Coast` = list("NY", "NJ", "CT"),
#'     `West Coast` = list("WA", "OR", "CA"),
#'     `Midwest` = list("MN", "WI", "IA")
#'   )
#' )
#' # the output will automatically be used here
#' view_ui(close_after = 6)
#' }
#' 

view_ui <- function(x, close_after = 5) {
  if (missing(x)) {
    x <- .Last.value
  }
  
  if (!class(x)[1] %in% c("shiny.tag", "shiny.tag.list")) {
    stop(
      'expected an object of class "shiny.tag" or "shiny.tag.list"',
      call. = FALSE
    )
  }
  
  ui <- fluidPage(x)
  
  server <- function(input, output) {
    if (!is.null(close_after)){
      Sys.sleep(close_after)
      stopApp()
    }
  }
  
  app <- shinyApp(ui, server)
  
  runApp(
    appDir = app, 
    launch.browser = rstudioapi::viewer
  )
}



