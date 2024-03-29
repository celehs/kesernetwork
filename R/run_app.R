#' Run the Shiny Application
#'
#' @param Rdata_path path to Rdata files.
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @return A shiny application.
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  Rdata_path = NULL,
  onStart = NULL,
  options = list(), 
  enableBookmarking = "server",
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server(Rdata_path),
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}
