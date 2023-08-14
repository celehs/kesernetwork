#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_header_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"),
                   " Download",
                   icon = icon("download"),
                   class = "btn btn-primary header-button",
                   width = "100px",
                   style = "padding: 6px;",
                   title = "The cosine similarity of current network."
    ),
    bookmarkButton(
      label = "Bookmark", id = ns("bookmark"),
      class = "btn btn-primary header-button"
    ),
    actionButton(ns("instruct"), " About",
                 icon = icon("book"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction of the app."
    ),
    actionButton(ns("help"), " Tutorial",
                 # icon = icon("question"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction tour."
    )
 
  )
}
    
#' name_of_module1 Server Functions
#'
#' @noRd 
mod_header_server <- function(id, sidebar_close){
  moduleServer( id, function(input, output, session){
    ns <- NS(id)
    
    steps = data.table::fread(app_sys("app/doc/steps.tsv"))

    observeEvent(input$help, {
      if (!sidebar_close) {
        shinydashboardPlus::updateSidebar("sidebar", session = session)
      }
      rintrojs::introjs(session,
                        options = list(
                        steps = steps[, -1],
                        showBullets = FALSE
                        )
      )
    })

    observeEvent(input$bookmark, {
      session$doBookmark()
    })
 
  })
}

