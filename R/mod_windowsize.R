#' Module for window size
#'
#' @description Module for using JS and get the size of the window refer to https://stackoverflow.com/questions/47424498/utilizing-window-size-within-shiny-module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
windowSizeUI <- function(id, label = "My Plot") {
  ns <- NS(id)
  dimensionId <- ns("dimension")
  tagList(
    tags$head(tags$script(sprintf("
      var dimensionId = '%s';
      var dimension = [0, 0];

      $(document).on('shiny:connected', function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange(dimensionId, dimension);
      });

      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange(dimensionId, dimension);
      });
    ", dimensionId)))
  )
}



#' name_of_module1 Server Functions
#'
#' @noRd 
windowSizeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    reactive(c(as.numeric(input$dimension)))
  })
}

