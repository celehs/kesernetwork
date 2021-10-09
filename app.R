
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(highcharter)
library(igraph)
library(Matrix)
library(plotly)
library(reactable)
library(readr)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyhelper)
library(shinyWidgets)
library(stringr)
library(visNetwork)
library(yaml)


load("kesernetwork.RData")
source("func/utils.R")
source("func/network.R")
source("func/sunburst.R")
source("func/circularStatic.R")
options(stringsAsFactors = FALSE)

#  ui ---------------------------
ui <- function(request) {
  tagList(
  dashboardPage(
    dashboardHeader(
      title = "KESER Network",
      leftUi = tagList(
        includeCSS("www/style.css"),
        dropdownButton(
          inputId = "controls",
          label = "Controls",
          icon = icon("sliders"),
          status = "primary",
          circle = FALSE,
          selectInput("network_layout", "The layout of network",
            choices = c("layout_nicely", "layout_with_mds", "layout_with_lgl"), 
            selected = "layout_nicely"
          ) %>% 
            helper(type = "markdown",
                   title = "The layout of network",
                   content = "helper_layout",
                   size = "m"),
          selectInput("Focus",
            label = "Choose one node to focus on:",
            choices = "All", width = "100%"
          ) %>% 
            helper(type = "markdown",
                   title = "Focus on node",
                   content = "helper_focuson",
                   size = "s"),
          sliderInput("scale_id", "Focus scale (zoomlevel):", width = "100%", 
                      min = 1, max = 10, value = 5),
          sliderInput("slider_h", "Graph height:",
            min = 100, max = 1500, value = 750, width = "100%"
          )
        ),
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
      ),
      titleWidth = "310pt"
    ),
    dashboardSidebar(
      id = "sidebar",
      collapsed = TRUE,
      width = "310pt",
      introjsUI(),
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
        helper(type = "markdown",
               colour = "white",
               title = "Knowledge network construction method",
               content = "helper_dataset",
               size = "m",
               style = "margin-right: 5px;"),
      id = "divselectmethod"),
      hr() %>% 
        helper(type = "markdown",
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
              helper(type = "markdown",
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
      ),
      minified = FALSE
    ),
    dashboardBody(
      shinybrowser::detect(),
      uiOutput("network"),
      bsModal(
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
                sliderTextInput("changeline", "max Text length on each line (set as 99 if not breaking lines:)",
                                choices = c(5, 10, 15, 20, 25, 99), selected = 10, grid = TRUE, width = "100%"
                ),
                pickerInput(
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
          helper(type = "markdown",
                 title = "The plots and tables",
                 content = "helper_tabs_nodeinfo",
                 size = "m")
      ),
      uiOutput("ui_selectedcluster"),
      bsModal(
        id = "instruction", title = "Instruction", trigger = "instruct",
        size = "large",
        includeMarkdown("doc/documentation.md")
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
           tags$img(src = "celehs_logo_40.png", 
                    title="CELEHS", height="40", 
                    class = "footer-logo")),
    tags$a(href = "https://www.verityresearch.org/", target = "_blank",
           tags$img(src = "VERITY_40.png", 
                    title="VERITY (BWH)", height="40", 
                    class = "footer-logo")),
    tags$a(href = "https://parse-health.org/", target = "_blank",
           tags$img(src = "parse_40.png", 
                    title="PARSE health", 
                    class = "footer-logo")),
  
  ), align = "center", class = "footer-bar")
  
  )#end tagList
}



# server ---------------------

server <- function(input, output, session) {
  showNotification("Click 'Help' button to open step-by-step instructions.",
    duration = 3, type = "warning"
  )
  
  steps = read_tsv("doc/steps.tsv")
  attrs = yaml.load_file("www/style.yaml")
  
  attrs = lapply(attrs, as.data.frame)
  
  observeEvent(input$help, {
    if (!input$sidebar) {
      updateSidebar("sidebar")
    }
    introjs(session,
      options = list(
        steps = steps[, -1],
        showBullets = FALSE
      )
    )
  })
  
  observe_helpers(help_dir = "doc")
  
  
  ####################  input   #################################################

  maxHeight <- reactive({shinybrowser::get_height()})
  
  method <- reactive({ input$selectmethod })
  
  selected_rows <- reactive({df_input()$nodeID[input$df_table_rows_selected]})
  
  selected_nodes <- eventReactive(input$goButton, {
      input$inCheckboxGroup2
    }, ignoreNULL = FALSE
  )

  cluster <- eventReactive(input$goButton, {
      input$cluster
    }, ignoreNULL = FALSE
  )
  
  hide_labels <- eventReactive(input$goButton, {
      input$hide_labels
    }, ignoreNULL = FALSE
  )
  
  node_id <- reactive({ 
    if (is.character(input$current_node_id$nodes[[1]])){
      if(strsplit(input$current_node_id$nodes[[1]], ":", fixed = TRUE)[[1]][1] == "cluster"){
        NULL
      }
      else {
        input$current_node_id$nodes[[1]] 
      }
    }
    })

  CosMatrix <- reactive({ cos.list[[method()]] })

  interested <- colnames(cos.list[[1]])

  not_intereted <- reactive({ setdiff(rownames(CosMatrix()), interested) })

  ###############  DT input table   ############################################
  
  df_input <- reactive({
    ord <- gsub("\\:.+", "", rownames(CosMatrix()), perl = TRUE)
    ord <- factor(ord, levels = c("PheCode", "RXNORM", "CCS", "LOINC", "ShortName", "Other lab"))
    df <- data.frame(
      "nodeID" = rownames(CosMatrix()),
      "Description" = str_wrap(dict.combine$Description[match(rownames(CosMatrix()), dict.combine$Variable)], width = 20),
      "type" = ord,
      "id" = gsub(".+\\:", "", rownames(CosMatrix()), perl = TRUE)
    )
    df$istarget = "others"
    df$istarget[df$nodeID %in% colnames(CosMatrix())] <- "target"
    df <- df[with(df, order(type, id)), ]
    df <- df[with(df, order(istarget, decreasing = TRUE)), ]
  })

  output$ui_table <- renderUI({
    withSpinner(
      DTOutput("df_table")
    ,type = 6)
  })

  output$df_table <- renderDT(datatable({
        df_input()[, c(1:2, 5)]
      }, rownames = FALSE,
      extensions = c("Buttons", "Select"),
      options = list(
        paging = FALSE,
        scrollY = "300px",
        scrollCollapse = TRUE,
        dom = "Bfrtip",
        select = list(
          style = "multiple", items = "row"
        ),
        buttons = list("selectNone")
      ),
      selection = "none",
      escape = FALSE
    ) %>%
    formatStyle(
      columns = colnames(df_input),
      backgroundColor = "#222d32", color = "white"
    ) %>% formatStyle(
      'istarget',
      target = 'row',
      backgroundColor = styleEqual(c('target', "others"), c('#D5F5E3', 'lightgray'))
    ), server = FALSE)
  
  ##############  sidebar ######################################################

  observeEvent(selected_rows(), {
    updateCheckboxCandidate(selected_rows(), CosMatrix, session, dict.combine)
  })
  
  
  observeEvent(input$inCheckboxGroup2, {
    updateCheckboxInput(
      inputId = "hide_labels",
      value = ifelse(length(input$inCheckboxGroup2) < 3, FALSE, TRUE)
    )
  })

  observeEvent(input$goButton, {
    if (length(selected_nodes()) >= 10) {
      showNotification(paste("You've chosen ", length(selected_nodes()), 
                             " nodes. It will take a while to finish plotting..."),
        duration = 3, type = "message"
      )
    }
  })
  
  ######################  network  #############################################
  
  output$network <- renderUI({
    if (length(selected_nodes()) > 0) {
      shinycssloaders::withSpinner(
        visNetworkOutput("network_proxy_nodes",
                         height = paste0(max(input$slider_h, (maxHeight()) - 65), "px")
        ),
        type = 6
      )
    } else {
      div(tags$span("Try to click some rows in "),
          tagList(icon("table")),
          tags$spa(" to specify your nodes"),
          align = "center",
          style = "padding-top: 40px; 
                   font-size: 30px; 
                   color: white; 
                   height: 1000px;"
      )
    }
  })

  draw.data <- eventReactive(selected_nodes(), {
    if (length(selected_nodes()) != 0) {
      input.correct <- selected_nodes()[1:min(50, length(selected_nodes()))]
      dataNetwork(input.correct, CosMatrix(), dict.combine, attrs)
    } else {
      NA
    }
  })
  
  output$network_proxy_nodes <- renderVisNetwork({
    plot_network(selected_nodes(), cluster(), draw.data(), hide_labels(), 
                 CosMatrix(), dict.combine, attrs, input$network_layout)
  })
  
  ##################### info for clicked node   ################################
  
  observeEvent(input$current_node_id$nodes[[1]], {
    if(strsplit(input$current_node_id$nodes[[1]], ":", fixed = TRUE)[[1]][1] == "cluster"){
      toggleModal(session, "selectedcluster", toggle = "open")
    } else {
      toggleModal(session, "selectednode", toggle = "open")
    }
  })
  
  output$clicked_node_info <- renderUI({
    clickedNodeText(node_id(), dict.combine)
  })
  
  
  #################### info for clicked group   ################################
  
  selected_group <- reactive({
    if (!is.null(input$current_node_id$nodes[[1]])){
      gsub("cluster:", "", input$current_node_id$nodes[[1]], fixed = TRUE)
    }
  })
  
  output$ui_selectedcluster <- renderUI({
      bsModal(
        id = "selectedcluster", title = paste("Group: ", selected_group()), 
        trigger = FALSE,
        size = "large",
        uiOutput("clusterinfor")
      )
  })
  
  output$clusterinfor <- renderUI({
      reactableOutput("tb_selectedgroup")
  })
  
  output$tb_selectedgroup <- renderReactable({
    df_nodes <- draw.data()[[2]]
    df_cluster <- df_nodes[df_nodes$group == selected_group(), c("id", "label", "title")]
    reactable(df_cluster[, 1:2], 
              details = function(index) {
                title <- df_cluster[index, "title", drop = FALSE]
                datatable(title, escape = FALSE, rownames = FALSE, 
                          options = list(dom = "t", ordering = FALSE), 
                          width = "100%", height = "200px")
              })
  })

  
  ########################  plots for clicked nodes   ##########################
  
  ## Generate sunburst plot using plotly =======================================
  output$sun_ui <- renderUI({
    shinycssloaders::withSpinner(
    plotlyOutput("sun",
      width = "auto",
      height = paste0(input$scale_sungh, "px")
    )
    , type = 6
    )
  })

  output$sun <- renderPlotly({
    changeline <- input$changeline
    rotatelabel <- input$rotatelabel
    scale_sungh <- input$scale_sungh
    sunburstPlot(
      thr_cos = 0.01,
      changeline, rotatelabel, scale_sungh,
      node_id(), CosMatrix(), dict.combine, attrs$cap_color
    )
  })

  ## Generate circular plot using ggplot =======================================
  output$circularplot <- renderUI({
      div(plotOutput("circular",
        width = "100%",
        height = "700px"
      ), align = "center")
  })

  output$circular <- renderPlot({
    circularBar(
      thr_cos_pop = 0.01,
      node_id(), CosMatrix(), dict.combine, attrs
    )
  })
  
  ##################  addButton   ##############################################
  
  observeEvent(node_id(), {
    if ((!node_id() %in% selected_nodes())){
    output$ui_addbutton <- renderUI({
      div(actionButton("addButton", "Add to candidates", 
                       class = "btn-primary active", width = "157px"),
          align = "center", style = "margin-top: 23px;"
      )
    })
    } else {
      ""
    }
  })

  observeEvent(input$addButton, {
    updateCheckboxCandidate(c(selected_nodes(), node_id()),
                            CosMatrix, session, dict.combine)
  })

  output$downloadData <- WriteData(selected_nodes(), draw.data())
  
  #################  more info button  #########################################
  
  observeEvent(node_id(), {
    cap <- dict.combine$Capinfo[dict.combine$Variable == node_id()]
    href = switch(match(cap, c("CCS", "Lab", "PheCode", "RXNORM")), 
                  "https://hcup-us.ahrq.gov/toolssoftware/ccs_svcsproc/ccssvcproc.jsp",
                  "https://loinc.org/multiaxial-hierarchy/",
                  "https://phewascatalog.org/phecodes_icd10cm",
                  "https://mor.nlm.nih.gov/RxNav/")
    output$ui_moreinfo <- renderUI({
      div(actionButton("infoButton",
                       class = "btn-primary active", width = "157px",
                       tags$a("More information", 
                              href = href, 
                              target = "_blank")
      ), align = "center", style = "margin-top: 5px;")
    })
  })
  
  
  ####################  PheCode  add ICD info  #################################

  observeEvent(node_id(), {
    if (node_id() %in% phecode$Phecode) {
      phe_id <- gsub(".+:", "", node_id(), perl = TRUE)
      href <- paste0("http://app.parse-health.org/phecode-map/?phecode=", phe_id)
      output$tophecodemap <- renderUI({
        actionButton(
          inputId = "tomap", class = "btn-primary", width = "157px",
          tags$a("Phecode map to ICD", href = href, target = "_blank")
        )
      })
    } else {
      output$tophecodemap <- renderUI({
        ""
      })
    }
  })
  
  ###################  more tab   ##############################################
  
  observeEvent(node_id(), {
    if (node_id() %in% c(full_drug_del_med_proc$feature_id, med_proc$feature_id)){
      showTab(inputId = "hidden_tabs", target = "Drugs information")
    } else {
      hideTab(inputId = "hidden_tabs", target = "Drugs information")
    }
    if (node_id() %in% LabMap_0917$LOINC) {
      showTab(inputId = "hidden_tabs", target = "Lab information")
    } else {
      hideTab(inputId = "hidden_tabs", target = "Lab information")
    }
  })

  
  ####################  RxNorm  add drug info  #################################

  df_drugs <- reactive({
    drugs <- full_drug_del_med_proc[full_drug_del_med_proc$feature_id == node_id(), -1]
    drugs <- drugs[with(drugs, order(LocalDrugNameWithDose, Code)), ]
    drugs <- drugs[, apply(drugs, 2, function(x){sum(!is.na(x))>0})]
    if (length(dim(drugs)) == 0){
      drugs <- data.frame(Code = drugs, DrugClass = NA)
    }
    drugs[!duplicated(drugs), ]
  })

  output$reac_tb <- renderReactable({
    reactable({
      df_drugs()
    },
      groupBy = "Code",
    pagination = FALSE, height = maxHeight() - 450, rownames = FALSE
    )
  })

  output$ui_drugs <- renderUI({
    if (node_id() %in% full_drug_del_med_proc$feature_id){
        div(
          h4("Local Drug Information:"),
          reactableOutput("reac_tb")
        )
    } else {
        ""
    }
  })
  
  output$ui_med_proc <- renderUI({
    if (node_id() %in% med_proc$feature_id){
      div(
        h4("Medication Procedures:"),
        reactableOutput("tb_med_proc")
      )
    }
  })
  
  output$tb_med_proc <- renderReactable(reactable({
      med_proc[med_proc$feature_id == node_id(), -1]
    }, fullWidth = FALSE, columns = list(
      Description = colDef(minWidth = 400)
    )
  ))

  ############ lab info  #######################################################
  
  output$ui_lab <- renderUI({
    lab_info <- sort(LabMap_0917$LabChemTestName[LabMap_0917$LOINC == node_id()])
    box_info(title = "LabChemTestName:", 
             info = tags$ul(
                       lapply(lab_info, function(x){ tags$li(x) })
                     ), 
             height = maxHeight() - 450)
  })
  
  
  ############  controls for network  ##########################################

  observe({
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
  observe({
    if (input$Focus != "All") {
      id <- dict.combine$Variable[match(input$Focus, dict.combine$Description_s)]
      visNetworkProxy("network_proxy_nodes") %>%
        visFocus(id = id, scale = input$scale_id / 10)
    } else {
      visNetworkProxy("network_proxy_nodes")
    }
  })

  observeEvent(input$bookmark, {
    session$doBookmark()
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "server")
