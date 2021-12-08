#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import rintrojs
#' @noRd
app_server <- function(Rdata_path){
  
  server <- function(input, output, session) {
    load(Rdata_path)
    showNotification("Click 'Help' button to open step-by-step instructions.",
                     duration = 3, type = "warning"
    )
    
    steps = data.table::fread(app_sys("app/doc/steps.tsv"))
    attrs = yaml::yaml.load_file(app_sys("app/www/style.yaml"))
    
    attrs = lapply(attrs, as.data.frame)
    
    observeEvent(input$help, {
      if (!input$sidebar) {
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
    
    shinyhelper::observe_helpers(help_dir = app_sys("app/doc"))
    
    
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
        "Description" = stringr::str_wrap(dict.combine$Description[match(rownames(CosMatrix()), dict.combine$Variable)], width = 20),
        "type" = ord,
        "id" = gsub(".+\\:", "", rownames(CosMatrix()), perl = TRUE)
      )
      df$istarget = "others"
      df$istarget[df$nodeID %in% colnames(CosMatrix())] <- "target"
      df <- df[with(df, order(type, id)), ]
      df <- df[with(df, order(istarget, decreasing = TRUE)), ]
    })
    
    output$ui_table <- renderUI({
      shinycssloaders::withSpinner(
        DT::DTOutput("df_table")
        ,type = 6)
    })
    
    output$df_table <- DT::renderDT(DT::datatable({
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
      DT::formatStyle(
        columns = colnames(df_input),
        backgroundColor = "#222d32", color = "white"
      ) %>% DT::formatStyle(
        'istarget',
        target = 'row',
        backgroundColor = DT::styleEqual(c('target', "others"), c('#D5F5E3', 'lightgray'))
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
          visNetwork::visNetworkOutput("network_proxy_nodes",
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
    
    output$network_proxy_nodes <- visNetwork::renderVisNetwork({
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
        selected_group <- strsplit(input$current_node_id$nodes[[1]], ":", fixed = TRUE)[[1]][2]
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
      reactable::reactableOutput("tb_selectedgroup")
    })
    
    output$tb_selectedgroup <- reactable::renderReactable({
      df_nodes <- draw.data()[[2]]
      df_cluster <- df_nodes[df_nodes$group == selected_group(), c("id", "label", "title")]
      reactable::reactable(df_cluster[, 1:2], 
                details = function(index) {
                  title <- df_cluster[index, "title", drop = FALSE]
                  DT::datatable(title, escape = FALSE, rownames = FALSE, 
                            options = list(dom = "t", ordering = FALSE), 
                            width = "100%", height = "200px")
                })
    })
    
    
    ########################  plots for clicked nodes   ##########################
    
    ## Generate sunburst plot using plotly =======================================
    output$sun_ui <- renderUI({
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("sun",
                     width = "auto",
                     height = paste0(input$scale_sungh, "px")
        )
        , type = 6
      )
    })
    
    output$sun <- plotly::renderPlotly({
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
    
    observeEvent(selected_nodes(), {
      output$downloadData <- WriteData(selected_nodes(), draw.data())
    })
    
    
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
    
    output$reac_tb <- reactable::renderReactable({
      reactable::reactable({
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
          reactable::reactableOutput("reac_tb")
        )
      } else {
        ""
      }
    })
    
    output$ui_med_proc <- renderUI({
      if (node_id() %in% med_proc$feature_id){
        div(
          h4("Medication Procedures:"),
          reactable::reactableOutput("tb_med_proc")
        )
      }
    })
    
    output$tb_med_proc <- reactable::renderReactable(reactable::reactable({
      med_proc[med_proc$feature_id == node_id(), -1]
    }, fullWidth = FALSE, columns = list(
      Description = reactable::colDef(minWidth = 400)
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
        visNetwork::visNetworkProxy("network_proxy_nodes") %>%
          visNetwork::visFocus(id = id, scale = input$scale_id / 10)
      } else {
        visNetwork::visNetworkProxy("network_proxy_nodes")
      }
    })
    
    observeEvent(input$bookmark, {
      session$doBookmark()
    })
  }
  return(server)
}
