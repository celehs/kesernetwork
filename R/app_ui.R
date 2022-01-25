#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import htmltools
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      shinydashboardPlus::dashboardHeader(
      title = "KESER Network",
      leftUi = mod_header_ui("headerBtn"),
      titleWidth = "310pt",
      controlbarIcon = NULL
    ),
    shinydashboardPlus::dashboardSidebar(
      id = "sidebar",
      collapsed = FALSE,
      width = "310pt",
      minified = FALSE,
      rintrojs::introjsUI(),
      div(selectInput("selectmethod",
                      label = "Select data from:",
                      choices = list(
                        "VA network trained w VA & MGB data" = "VA_integrative",
                        "VA network trained w VA data only" = "VA_local",
                        "MGB network trained w MGB & VA data" = "MGB_integrative",
                        "MGB network trained w MGB data only" = "MGB_local"
                      ),
                      selected = "VA_integrative",
                      width = "100%"
      ) %>% 
        shinyhelper::helper(type = "markdown",
               colour = "white",
               title = "Knowledge network construction method",
               content = "helper_dataset",
               size = "m",
               style = "margin-right: 5px;"),
      id = "divselectmethod"),
      hr() %>% 
        shinyhelper::helper(type = "markdown",
               colour = "white",
               title = "The input table",
               content = "helper_input_table",
               size = "m",
               style = "margin-right: 5px;"),
      uiOutput("ui_table"),
      div(
        checkboxGroupInput("inCheckboxGroup2", "5 nodes selected:",
          choiceValues = c("PheCode:008.5", "PheCode:008.7", "PheCode:008",
                           "PheCode:010", "PheCode:031"),
          choiceNames = c(
            "bacterial enteritis (196 neighbors)",
            "intestinal infection due to protozoa (39 neighbors)",
            "intestinal infection (81 neighbors)",
            "tuberculosis (121 neighbors)",
            "diseases due to other mycobacteria (142 neighbors)"
          ),
          selected = c("PheCode:008.5", "PheCode:008.7", "PheCode:008", 
                       "PheCode:010", "PheCode:031"),
          width = "100%"
        ),
        id = "divcheckboxgroups"
      ),
      fluidRow(
        column(
          6,
          div(
            checkboxInput("cluster", "Cluster by groups", value = FALSE) %>% 
              shinyhelper::helper(type = "markdown",
                     colour = "white",
                     title = "Cluster by groups",
                     content = "helper_clustergroup",
                     size = "m"),
            checkboxInput("hide_labels", "Hide the labels", value = TRUE),
            id = "div_checkbox"
          )
        ),
        column(
          6,
          div(actionButton("goButton", "Show network",
            width = "150px",
            icon = tags$i(
              class = "far fa-play-circle",
              style = "font-size: 10px"
            ),
            class = "btn-success"
          ), align = "center")
        )
      )
    ),
    shinydashboard::dashboardBody(
      windowSizeUI("win"),
      uiOutput("network"),
      shinyBS::bsModal(
        id = "selectednode", title = "Node infomation", trigger = FALSE,
        size = "large",
        fluidRow(
          column(
            8,
            htmlOutput("clicked_node_info")
          ),
          column(
            3,
            uiOutput("ui_addbutton"),
            uiOutput("ui_moreinfo"),
            div(uiOutput("tophecodemap"), 
                align = "center", style = "margin-top: 5px;")
          )
        ),
        hr(),
        tabsetPanel(
          id = "hidden_tabs",
          tabPanel(
            title = "Circular plot",
            br(),
            h5("*Bar height reflects cosine similarity"),
            uiOutput("circularplot")
          ),
          tabPanel(
            title = "Sunburst plot",
            br(),
            fluidRow(
              column(
                6,
                shinyWidgets::sliderTextInput("changeline", "max Text length on each line (set as 99 if not breaking lines:)",
                                choices = c(5, 10, 15, 20, 25, 99), selected = 10, grid = TRUE, width = "100%"
                ),
                shinyWidgets::pickerInput(
                  inputId = "rotatelabel",
                  label = "The orientation of text inside sectors",
                  choices = c("Radial", "Tangential")
                )
              ),
              column(6, sliderInput("scale_sungh", "Graph height:",
                                    min = 500, max = 1000, value = 750, width = "100%"
              ))
            ),
            div(uiOutput("sun_ui"), align = "center")
          ),
          tabPanel(
            title = "Drugs information",
            br(),
            uiOutput("ui_drugs"),
            uiOutput("ui_med_proc")
          ),
          tabPanel(
            title = "Lab information",
            br(),
            uiOutput("ui_lab")
          )
        ) %>% 
          shinyhelper::helper(type = "markdown",
                 title = "The plots and tables",
                 content = "helper_tabs_nodeinfo",
                 size = "m")
      ),
      uiOutput("ui_selectedcluster"),
      shinyBS::bsModal(
        id = "instruction", title = "Instruction", trigger = "headerBtn-instruct",
        size = "large",
        includeMarkdown(app_sys("app/doc/documentation.md"))
      )
    )
  ),#end dashboardPage
  tags$footer(div(
    "Teams:",
    tags$a(href = "https://www.va.gov/", target = "_blank",
           tags$img(src = "https://s3-us-gov-west-1.amazonaws.com/content.www.va.gov/img/header-logo.png", 
                    title="VA", height="40", 
                    class = "footer-logo")),
    tags$a(href = "https://www.research.va.gov/programs/cipher.cfm", 
           tags$b("CIPHER"), 
           target = "_blank",
           class = "footer-text"),
    tags$a(href = "https://celehs.hms.harvard.edu/", target = "_blank",
           tags$img(src = "www/celehs_logo_40.png", 
                    title="CELEHS", height="40", 
                    class = "footer-logo")),
    tags$a(href = "https://www.verityresearch.org/", target = "_blank",
           tags$img(src = "www/VERITY_40.png", 
                    title="VERITY (BWH)", height="40", 
                    class = "footer-logo")),
    tags$a(href = "https://parse-health.org/", target = "_blank",
           tags$img(src = "www/parse_40.png", 
                    title="PARSE health", 
                    class = "footer-logo")),
  
  ), align = "center", class = "footer-bar")
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'kesernetwork'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

