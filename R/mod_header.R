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
    # includeCSS("www/style.css"),
    shinyWidgets::dropdownButton(
      inputId = "controls",
      label = "Controls",
      icon = icon("cog"),
      status = "primary",
      circle = FALSE,
      selectInput(ns("network_layout"), "The layout of network",
                  choices = c("layout_nicely", "layout_with_mds", "layout_with_lgl"), 
                  selected = "layout_nicely"
      ) %>% 
        shinyhelper::helper(type = "markdown",
                            title = "The layout of network",
                            content = "helper_layout",
                            size = "m"),
      selectInput(ns("Focus"),
                  label = "Choose one node to focus on:",
                  choices = "All", width = "100%"
      ) %>% 
        shinyhelper::helper(type = "markdown",
                            title = "Focus on node",
                            content = "helper_focuson",
                            size = "s"),
      sliderInput(ns("scale_id"), "Focus scale (zoomlevel):", width = "100%", 
                  min = 1, max = 10, value = 5),
      sliderInput(ns("slider_h"), "Graph height:",
                  min = 100, max = 1500, value = 750, width = "100%"
      )
    ),
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
    actionButton(ns("help"), " Help",
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
mod_header_server <- function(id, sidebar_close, selected_nodes, draw.data, dict.combine){
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

    observeEvent(selected_nodes(), {
      output$downloadData <- WriteData(selected_nodes(), draw.data)
    })

    observeEvent(input$bookmark, {
      session$doBookmark()
    })
    
    observeEvent(selected_nodes(), {
      if (length(selected_nodes()) != 0) {
        x <- dict.combine$Description_s[match(
          selected_nodes()[1:min(50, length(selected_nodes()))],
          dict.combine$Variable
        )]
        x <- c("All", x)
        updateSelectInput(session, "Focus", "Choose one node to focus on:",
                          choices = x, selected = "All"
        )
      }
    })
    observeEvent(c(input$Focus, input$scale_id), {
      if (input$Focus != "All") {
        focus_id <- dict.combine$Variable[match(input$Focus, dict.combine$Description_s)]
        visNetwork::visNetworkProxy("network_proxy_nodes") %>%
          visNetwork::visFocus(id = focus_id, scale = input$scale_id / 10)
      } else {
        visNetwork::visNetworkProxy("network_proxy_nodes")
      }
    })
    
    reactive({
      list("layout" = input$network_layout, 
           "slider_h" = input$slider_h)
    })
 
  })
}

