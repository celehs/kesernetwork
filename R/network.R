#' @importFrom visNetwork %>%

dataNetwork <- function(selected_nodes, df_edges, dict.combine, attrs){
  if(!is.null(df_edges)){
    saveRDS(df_edges, "df_edges.rds")
    attr_edges <- attrs$attr_edges
    attr_nodes_type <- attrs$attr_nodes_type
    attr_nodes_cap <- attrs$attr_nodes_cap
    
    df_edges$length <- abs(df_edges$weight * 1000)
    df_edges$title <- paste0(df_edges$center, "->", df_edges$to, ": ", round(df_edges$weight, 3))
    df_edges <- df_edges %>% dplyr::group_by(from, to) %>%
      dplyr::summarise(groupid = unique(groupid),
                       weight = paste(weight, collapse = "|"),
                       length = max(length),
                       title = paste(title, collapse = "<br>"))
    df_edges$edgetype <- "target-other"
    df_edges$edgetype[df_edges$from %in% selected_nodes &
                        df_edges$to %in% selected_nodes ] <- "target-target"
    df_edges <- left_join(df_edges, attr_edges, by = "edgetype")
    
    df_nodes <- data.frame(id = unique(c(df_edges$from, df_edges$to)))
    df_nodes <- left_join(df_nodes, dict.combine, by = c("id"))
    df_nodes$term[grepl("Group:", df_nodes$id)] <- dict.combine$groupdesc[match(gsub("Group:", "", df_nodes$id[grepl("Group:", df_nodes$id)]), dict.combine$groupid)]
    dict_group <- dict.combine[!duplicated(dict.combine$groupdesc) & (!is.na(dict.combine$groupdesc)), c("groupid", "groupdesc")]
    df_nodes$group1[grepl("Group:", df_nodes$id)] <- dict.combine$group1[match(gsub("Group:", "", df_nodes$id[grepl("Group:", df_nodes$id)], fixed = TRUE), dict.combine$groupid)]
    df_nodes$group1[df_nodes$group1 == "LP"] <- "LOINC"
    df_nodes$groupid[is.na(df_nodes$groupid)] <- df_nodes$id[is.na(df_nodes$groupid)]
    df_nodes$category[is.na(df_nodes$category)] <- dict.combine$category[match(df_nodes$group1[is.na(df_nodes$category)], dict.combine$group1)]
    df_nodes$label <- df_nodes$term
    df_nodes$label[!df_nodes$id %in% dict.combine$id] <- paste0("GROUP\n", df_nodes$term[!df_nodes$id %in% dict.combine$id])
    
    # df_nodes <- left_join(df_nodes, dict.combine[, c(1, 5, 7, 8, 4)], by = c("id" = "Variable"))
    # colnames(df_nodes) <- c("id", "label", "group", "group_title", "Cap")
    df_nodes$nodetype <- "other"
    df_nodes$nodetype[df_nodes$id %in% selected_nodes] <- "target"
    df_nodes$Cap <- gsub("^(\\w).+", "\\1", df_nodes$group1, perl = TRUE)
    df_nodes <- left_join(df_nodes, attr_nodes_type, by = "nodetype")
    df_nodes <- left_join(df_nodes, attr_nodes_cap, by = "Cap")
  
    # df_nodes$group[df_nodes$id %in% selected_nodes] <- df_nodes$label[df_nodes$id %in% selected_nodes]
  
    
    df_nodes$title = paste0("<b>ID: </b>",df_nodes$id,
                            "<br><b>Description: </b>",df_nodes$term,
                            "<br>", df_edges$title[match(df_nodes$id, df_edges$to)])
    df_nodes$title[!df_nodes$id %in% dict.combine$id] = paste0(df_nodes$groupid[!df_nodes$id %in% dict.combine$id],
                                                               "<br><b>Description: </b>",df_nodes$term[!df_nodes$id %in% dict.combine$id],
                                                               "<br>", df_edges$title[match(df_nodes$id[!df_nodes$id %in% dict.combine$id], df_edges$to)])
  
    
    text_freq = sapply(df_nodes$id, function(x){
      if(!is.na(dict.combine$marginal_pat_VA[match(x,dict.combine$id)])){
        paste0("<br><b>Patient prevalence: </b>", round(dict.combine$marginal_pat_VA[match(x,dict.combine$id)]/12600000,4),
               "<br><b>Ave count per patient: </b>", round(dict.combine$marginal_freq_VA[match(x,dict.combine$id)]/
                                                             dict.combine$marginal_pat_VA[match(x,dict.combine$id)],2))
        
      } else {
        ""
      }
    })
    
    df_nodes$title = paste0(df_nodes$title, text_freq)
    
    df_nodes$font.background <- NA
    df_nodes$font.color = df_nodes$color.background
    df_nodes$font.color[grepl("Group:", df_nodes$id)] = "Black"
  
    # df_nodes$font.background[is.na(df_nodes$font.background)] <- ""
  
    df_nodes$shape <- "box"
    df_nodes$shape[df_nodes$nodetype == "target"] <- "star"
    df_nodes$shape[df_nodes$id %in% dict.combine$groupid] <- "ellipse"
    df_nodes$shape[df_nodes$id %in% dict.combine$id] <- "dot"
  
    return(list(df_edges, df_nodes))
  } else {
    return(list(data.frame(), data.frame(), data.frame()))
  }
}


add_attr_network <- function(p, layout = "layout_nicely"){
  p %>%
    visNetwork::visNodes(shadow = list(enabled = TRUE, size = 4, x = 3, y = 3)) %>%
    visNetwork::visEdges(physics = FALSE,
    smooth = FALSE,
    hoverWidth = 2.5) %>%
    visNetwork::visOptions(
      highlightNearest = list(
      enabled = TRUE,
      degree = 1,
      hover = FALSE,
      hideColor = "rgba(200,200,200,0.2)")) %>%
    visNetwork::visInteraction(hover = TRUE) %>%
    visNetwork::visIgraphLayout(layout = layout,
                    physics = FALSE,
                    smooth = FALSE,
                    type = "square") %>%
    visNetwork::visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                    ;}") %>%
    visNetwork::visLayout(randomSeed = 10) # to have always the same network
}


plot_network <- function(df_edges, center_nodes, hide_labels, 
                         dict.combine, attrs, layout = "layout_nicely"){
  if(nrow(df_edges) > 0){
    print(center_nodes)
    draw.data = dataNetwork(center_nodes, df_edges, dict.combine, attrs)
    df_edges = draw.data[[1]]
    df_nodes = draw.data[[2]]
    print(nrow(df_edges))
    
    if(hide_labels){
      df_nodes$label <- ""
      df_nodes$font.size[df_nodes$iscenter == "center"] <- 50
      df_nodes$font.size[df_nodes$iscenter == "other"] <- 30
      df_nodes$font.background <- NA
    }
    saveRDS(df_nodes, "df_nodes.rds")
    print(head(df_nodes))
    print(table(df_nodes$category))
    
    legend_nodes <- df_nodes[, c("category", "color.background")]
    legend_nodes <- legend_nodes[!duplicated(legend_nodes), ]
    legend_nodes$shape <- "dot"
    legend_nodes$font.color <- "white"
    legend_nodes$size <- 10
    colnames(legend_nodes) <- c("label", "color.background", "shape", "font.color", "size")
    
    legend_nodes$physics <- FALSE
    
    legend_nodes$font.size <- 10
    
    
    p <- visNetwork::visNetwork(df_nodes, df_edges, width = "100%",height = "100%", 
                    shape = "box")  %>%
      visNetwork::visLegend(width = 0.09, position = "right",
                            addNodes = legend_nodes,
                            useGroups = FALSE, zoom = TRUE,
                            stepX = 150, stepY = 75,ncol=1)
    add_attr_network(p, layout)
    
  } else {
    visNetwork::visNetwork(data.frame(), data.frame(), width = "100%",
               main = paste("Try to click some rows in",tagList(icon("table")),"to specify your nodes"))
  }
}
