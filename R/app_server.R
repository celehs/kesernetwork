#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import rintrojs
#' @import shiny
#' @import shinyBS
#' @import rintrojs
#' @noRd
app_server <- function(Rdata_path, Uniq_id){
  
  server <- function(input, output, session) {
    if(isTruthy(Rdata_path)){
      load(Rdata_path)
    }
    
    showNotification("Click 'Help' button to open step-by-step instructions.",
                     duration = 3, type = "warning")
    
    mod_header_server("headerBtn", input$sidebar)

    attrs = yaml::yaml.load_file(app_sys("app/www/style.yaml"))
    
    attrs = lapply(attrs, as.data.frame)
    attrs$legend_groups$label = stringr::str_replace(attrs$legend_groups$label, 'CCS', 'ProcedureCode')
    attrs$cap_color$name = stringr::str_replace(attrs$cap_color$name, 'CCS', 'ProcedureCode')
    attrs$attr_nodes_cap$Cap_label = stringr::str_replace(attrs$attr_nodes_cap$Cap_label, 'CCS', 'ProcedureCode')
    
    # color_dict = data.frame(code_type = c('PheCode', 'LOINC', 'RXNORM', 'ProcedureCode','Procedure Code',
    #                                       'VA Lab Code', 'VA Lab Group', 'Lab'),
    #                         color = c('#8AE6FF', '#76E43F', '#B39DDB', '#EF5350', '#EF5350',
    #                                   '#30E3A4', '#FAE48B', '#30E3A4'))
    ##Tianxi's choice:
    color_dict = data.frame(code_type = c('PheCode', 'LOINC', 'RXNORM', 'ProcedureCode','Procedure Code',
                                          'VA Lab Code', 'VA Lab Group', 'Lab'),
                            color = c('#00C6F2', '#30E3A4', '#C7A8F0', '#F20C51', '#F20C51',
                                      '#30E3A4', '#FAE48B', '#30E3A4'))
    
    ## order: procedure, loinc(lab), lab, phecode, rxnorm, lab
    attrs$attr_nodes_cap$color.background = 
      attrs$attr_nodes_cap$color.highlight.background =
      attrs$attr_nodes_cap$color.hover.background =
      attrs$attr_nodes_cap$color.border =
      color_dict$color[match(attrs$attr_nodes_cap$Cap_label, color_dict$code_type)] 
    
    attrs$legend_groups$color[which(attrs$legend_groups$label %in% color_dict$code_type)] = 
      color_dict$color[match(attrs$legend_groups$label[which(attrs$legend_groups$label %in% color_dict$code_type)], color_dict$code_type)] 
    
    attrs$cap_color$color =  color_dict$color[match(attrs$cap_color$name, color_dict$code_type)] 
    
    
    shinyhelper::observe_helpers(help_dir = app_sys("app/doc"))
    
    
    # input ====
    
    winsize <- windowSizeServer("win")
    
    method <- reactive({ input$selectmethod })
    
    url_vars <- reactive({ session$clientData$url_search })
    
    uniq_id <- reactive({
      if(!is.null(Uniq_id)){
        utils::read.csv(Uniq_id, header = TRUE, colClasses = c("character", "character"))
      }
    })
    
    url_node <- reactive({
      if(grepl('uqid=', url_vars())){
        id = gsub(".+?uqid=(.+)", "\\1", url_vars(), perl = TRUE)
        id = strsplit(id, "&")[[1]]
        print(id)
        print(id %in% colnames(CosMatrix()))
        print("url_node")
        print(id)
        print(uniq_id())
        print(uniq_id()$id[uniq_id()$uqid == id])
        if ((id %in% colnames(CosMatrix()))[1]) {
          id
        } else if(!is.null(uniq_id())){
          if((id %in% uniq_id()$uqid)[1]){
            uniq_id()$id[uniq_id()$uqid == id]
          }
        }
      }
    })
    
    selected_nodes <- eventReactive(input$gobutton, {
      print(url_node())
      if(!isTruthy(input$gobutton)){
        if(isTruthy(url_node())){
          url_node()
        } else {
          c("PheCode:008.5", "PheCode:008.6", "PheCode:008.7",
            "PheCode:010", "PheCode:031")
      }}
        else{
        input$inCheckboxGroup2
      }
    }, ignoreNULL = FALSE)
    
    cluster <- eventReactive(input$goButton, {
      input$cluster
    }, ignoreNULL = FALSE
    )
    
    hide_labels <- eventReactive(input$goButton, {
      input$hide_labels
    }, ignoreNULL = FALSE
    )
    
    node_id <- reactive({
      req(input$current_node_id)
      if (is.character(input$current_node_id$nodes[[1]])){
        if(strsplit(input$current_node_id$nodes[[1]], ":", fixed = TRUE)[[1]][1] == "cluster"){
          NULL
        }
        else {
          input$current_node_id$nodes[[1]] 
        }
      }
    })
    
    CosMatrix <- reactive({
      req(Rdata_path)
      print(dim(cos.list[[method()]]))
      cos.list[[method()]] 
    })
    
    
    # DT input table ====
    
    df_input <- reactive({
      ids <- unique(colnames(CosMatrix()),rownames(CosMatrix()))
      ord <- gsub("\\:.+", "", ids, perl = TRUE)
      ord <- factor(ord, levels = c("PheCode", "RXNORM", "ProcedureCode", "LOINC", "ShortName", "Other lab"))
      df <- data.frame(
        "nodeID" = ids,
        "Description" = stringr::str_wrap(dict.combine$Description[match(ids, dict.combine$Variable)], width = 20),
        "type" = ord,
        "id" = gsub(".+\\:", "", ids, perl = TRUE)
      )
      df$istarget = "others"
      df$istarget[df$nodeID %in% colnames(CosMatrix())] <- "target"
      df <- df[with(df, order(type, id)), ]
      df <- df[with(df, order(istarget, decreasing = TRUE)), ]
    })
    
    output$ui_table <- renderUI({
      if(isTruthy(Rdata_path)){
        shinycssloaders::withSpinner(
          DT::DTOutput("df_table")
          ,type = 6)
      } else {
        h4("set 'Rdata_path' in run_app(Rdata_path = 'path to Rdata of kesernetwrok')")
      }
      
    })
    
    selected_rows = reactive({
      if(!isTruthy(input$goButton)){
        c(1,4:7)
      }
    })
    
    output$df_table <- DT::renderDT(DT::datatable({
      df_input()[, c(1:2, 5)]
    }, rownames = FALSE,
    options = list(
      paging = FALSE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      dom = "Bfrtip"
    ),
    selection = list(mode = 'multiple', 
                     selected = selected_rows(), 
                     target = 'row'),
    escape = FALSE
    ) %>%
      DT::formatStyle(
        columns = colnames(df_input),
        backgroundColor = "#222d32", color = "white"
      ) %>% DT::formatStyle(
        'istarget',
        target = 'row',
        backgroundColor = DT::styleEqual(c('target', "others"), c('#D5F5E3', 'lightgray'))
      ), server = TRUE)
    
    # sidebar ====
    
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
    
    # network ====
    
    output$network <- renderUI({
      if (isTruthy(Rdata_path) & (length(selected_nodes()) > 0)) {
        # req(controls())
        req(winsize()[2])
        shinycssloaders::withSpinner(
          visNetwork::visNetworkOutput("network_proxy_nodes",
                           height = paste0(winsize()[2] - 65, "px")
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
      plot_network(selected_nodes(), draw.data(), hide_labels(), 
                   CosMatrix(), dict.combine, attrs)
    })
    
    # info for clicked node ====
    
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
    
    
    # info for clicked group ====
    
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
    
    
    
    # sunburst ====
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
    
    # circular plot ====
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
    
    # addButton ====
    
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
    
    # Update checkboxinput based on selected rows in table ====
    checkboxUpdateBySelectedRows <- function(inputid, rows, input_table, session){
      if(!isTruthy(rows)){
        x <- character(0)
        updateCheckboxGroupInput(session, inputid,
                                 "0 node(s) selected:",
                                 choices = x,
                                 selected = x)
      }
      if(length(rows)!=0){
        x = input_table$nodeID[rows]
        x.name = input_table$Description[rows]
        updateCheckboxGroupInput(session, inputid,
                                 label = paste(length(x), "node(s) selected:"),
                                 choiceValues = x,
                                 choiceNames = paste0(x,": ",x.name),
                                 selected = x
        )
      }
    }
    observeEvent(input$df_table_rows_selected, {
      checkboxUpdateBySelectedRows("inCheckboxGroup2", 
                                   input$df_table_rows_selected, 
                                   df_input(), session)
    })
    
    observeEvent(input$deselect, {
      print("input$deselect")
      
      DT::reloadData(
        proxy,
        resetPaging = TRUE,
        clearSelection = c("all"))
      
      x <- character(0)
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               "0 node(s) selected",
                               choices = x,
                               selected = x)
      
    })
    
    observeEvent(url_node(), {
      print("url_node()")
      print(url_node())
      print(isTruthy(url_node()))
      if(isTruthy(url_node())){
        print("reset")
        
        if (input$sidebar) {
          shinydashboardPlus::updateSidebar("sidebar", session = session)
        }
        
        DT::reloadData(
          proxy,
          resetPaging = TRUE,
          clearSelection = c("all"))
        
        x <- character(0)
        updateCheckboxGroupInput(session, "inCheckboxGroup2",
                                 "0 node(s) selected",
                                 choices = x,
                                 selected = x)
      }
    })
    
    proxy <- DT::dataTableProxy('df_table')
    
    observeEvent(input$selectmethod, {
      print("input$selectmethod")
      
      DT::reloadData(
        proxy,
        resetPaging = TRUE,
        clearSelection = c("all"))
      
      checkboxUpdateBySelectedRows("inCheckboxGroup2", 
                                   input$df_table_rows_selected, 
                                   df_input(), session)
      
    })
    
    # Download ====
    observeEvent(selected_nodes(), {
      output$`headerBtn-downloadData` <- WriteData(selected_nodes(), draw.data())
    })
    
    
    # more info button ====
    
    observeEvent(node_id(), {
      cap <- dict.combine$Capinfo[dict.combine$Variable == node_id()]
      href = switch(match(cap, c("ProcedureCode", "Lab", "PheCode", "RXNORM")), 
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
    
    
    # PheCode add ICD info ====
    
    observeEvent(node_id(), {
      if (node_id() %in% phecode$Phecode) {
        phe_id <- gsub(".+:", "", node_id(), perl = TRUE)
        href <- paste0("https://shiny.parse-health.org/phecodemap/?phecode=", phe_id)
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
    
    # more tab ====
    
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
    
    
    # RxNorm add drug info ====
    
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
      pagination = FALSE, height = winsize()[2] - 450, rownames = FALSE
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
    
    # lab info ====
    
    output$ui_lab <- renderUI({
      lab_info <- sort(LabMap_0917$LabChemTestName[LabMap_0917$LOINC == node_id()])
      box_info(title = "LabChemTestName:", 
               info = tags$ul(
                 lapply(lab_info, function(x){ tags$li(x) })
               ), 
               height = winsize()[2] - 450)
    })
    
    
    # btn back to VA ====
    
    observeEvent(node_id(), {
      if ((node_id() %in% url_node())[1]) {
        uqid <- uniq_id()$uqid[uniq_id()$id == node_id()]
        # href <- paste0("https://phenomics-dev.va.ornl.gov/cipher/phenotype-viewer?uqid=", uqid)
        href <- paste0("https://celehs.hms.harvard.edu/DEMO/CIPHER_VA/", uqid)
        output$toVA <- renderUI({
          
          actionButton("tova",
                       class = "btn-primary active", width = "157px",
                       icon = icon("share"),
                       title = "Link back to CIPHER.",
                       tags$a("View in CIPHER", 
                              href = href, 
                              target = "_blank"))
          
          
          
        })
      } else {
        output$toVA <- renderUI({
          ""
        })
      }
    })
    
    
  }
  return(server)
}
