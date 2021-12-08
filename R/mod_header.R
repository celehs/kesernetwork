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
    downloadButton("downloadData",
                   " Download",
                   icon = icon("download"),
                   class = "btn btn-primary header-button",
                   width = "100px",
                   style = "padding: 6px;",
                   title = "The cosine similarity of current network."
    ),
    bookmarkButton(
      label = "Bookmark", id = "bookmark",
      class = "btn btn-primary header-button"
    ),
    actionButton("instruct", " About",
                 icon = icon("book"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction of the app."
    ),
    actionButton("help", " Help",
                 icon = icon("question"),
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
mod_header_server <- function(id, sidebar_close, selected_nodes, draw.data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    steps = data.table::fread(app_sys("app/doc/steps.tsv"))
    
    observeEvent(input$help, {
      if (!sidebar_close) {
        print("updateSidebar")
        shinydashboardPlus::updateSidebar("sidebar", session = session)
      }
      rintrojs::introjs(session,
                        options = list(
                          steps = steps[, -1],
                          showBullets = FALSE
                        )
      )
    })
    
    observeEvent(selected_nodes(), {
      output$downloadData <- WriteData(selected_nodes(), draw.data())
    })
    
    observeEvent(input$bookmark, {
      session$doBookmark()
    })
    
    observeEvent(input$instruct, {
      toggleModal(session, "instruction", toggle = "open")
    })
 
  })
}
    
## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_ui_1")
    
## To be copied in the server
# mod_name_of_module1_server("name_of_module1_ui_1")
