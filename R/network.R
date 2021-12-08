#' @importFrom visNetwork %>%

dataNetwork <- function(selected_nodes, CosMatrix, dict.combine, attrs){

  attr_edges <- attrs$attr_edges
  attr_nodes_type <- attrs$attr_nodes_type
  attr_nodes_cap <- attrs$attr_nodes_cap

  df_edges <- NULL
  for (i in selected_nodes){
    to = getNeighbors(i, CosMatrix)
    cor = switch((i %in% colnames(CosMatrix)) + 1, 
                 CosMatrix[i, to, drop = TRUE], 
                 CosMatrix[to, i, drop = TRUE])
    df_edges <- rbind(df_edges, data.frame("from" = i,
                                           "to" = to,
                                           "corvalue" = cor))
  }
  
  df_edges <- df_edges[df_edges$from != df_edges$to, ]

  df_edges$ends <- paste0(df_edges$from, ";",df_edges$to)
  df_edges$ends <- sapply(df_edges$ends, function(x){
    paste(sort(strsplit(x, ";", fixed = T)[[1]]), collapse = ";")
  })

  df_edges <- df_edges[!duplicated(df_edges$ends), ]
  df_edges <- df_edges[, -4]

  df_edges$length <- abs(df_edges$corvalue)^(-1.1)*10
  df_edges$title <- paste0(df_edges$from,"<b> &rarr; </b>", df_edges$to)
  df_edges$edgetype <- "target-other"
  df_edges$edgetype[df_edges$from %in% selected_nodes &
                      df_edges$to %in% selected_nodes ] <- "target-target"


  df_edges <- left_join(df_edges, attr_edges, by = "edgetype")

  df_nodes <- data.frame(id = unique(c(df_edges$from, df_edges$to)))
  df_nodes <- left_join(df_nodes, dict.combine[, c(1, 5, 7, 8, 4)], by = c("id" = "Variable"))
  colnames(df_nodes) <- c("id", "label", "group", "group_title", "Cap")
  df_nodes$nodetype <- "other"
  df_nodes$nodetype[df_nodes$id %in% selected_nodes] <- "target"

  df_nodes <- left_join(df_nodes, attr_nodes_type, by = "nodetype")
  df_nodes <- left_join(df_nodes, attr_nodes_cap, by = "Cap")

  df_nodes$group[df_nodes$id %in% selected_nodes] <- df_nodes$label[df_nodes$id %in% selected_nodes]

  df_nodes$shape <- "box"
  df_nodes$shape[df_nodes$id %in% colnames(CosMatrix)] <- "ellipse"

  df_nodes$title = paste0("<b>ID: </b>",df_nodes$id,
                          "<br><b>Description: </b>",dict.combine$Description[match(df_nodes$id,dict.combine$Variable)],
                          "<br><b>Group: </b>", df_nodes$group_title)
  text_freq = sapply(df_nodes$id, function(x){
    if(!is.na(dict.combine$marginal_pat_VA[match(x,dict.combine$Variable)])){
      paste0("<br><b>Patient prevalence: </b>", round(dict.combine$marginal_pat_VA[match(x,dict.combine$Variable)]/12600000,4),
             "<br><b>Ave count per patient: </b>", round(dict.combine$marginal_freq_VA[match(x,dict.combine$Variable)]/
                                                           dict.combine$marginal_pat_VA[match(x,dict.combine$Variable)],2))
      
    } else {
      ""
    }
  })
  
  df_nodes$title = paste0(df_nodes$title, text_freq)
  
  df_nodes$font.background[is.na(df_nodes$font.background)] <- ""

  df_groups = df_nodes[, c("group", "color.background")]
  df_groups <- df_groups[!duplicated(df_groups),]

  return(list(df_edges, df_nodes, df_groups))
}


add_attr_network <- function(p, layout = "layout_nicely"){
  p %>%
    visNetwork::visNodes(shadow = list(enabled = TRUE, size = 4, x = 3, y = 3)) %>%
    visNetwork::visEdges(physics = FALSE,
    smooth = FALSE,
    hoverWidth = 2.5) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T,
                                       degree = 1,
                                       hover = FALSE,
                                       hideColor = "rgba(200,200,200,0.2)"),
               selectedBy = list(`variable` = "Cap_label",
                                 `multiple` = TRUE,
                                 `main` = "Select by group"),
               collapse = FALSE) %>%
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


plot_network <- function(s, cluster, draw.data, hide_label, CosMatrix, dict.combine, attrs, layout, gravitationalConstant){

  selected_nodes = s[1:min(50,length(s))]
    root.node = match(selected_nodes, rownames(CosMatrix))
    df_edges = draw.data[[1]]
    df_nodes = draw.data[[2]]
    df_groups = draw.data[[3]]
    print(nrow(df_edges))
    if(hide_label){
      df_nodes$label <- ""
      df_nodes$font.size[df_nodes$nodetype == "target"] <- 50
      df_nodes$font.size[df_nodes$nodetype == "other"] <- 30
      df_nodes$font.background <- NA
      df_nodes$label[df_nodes$shape == "box"] <- "        "
      attrs$legend_groups$size[1:7] <- 10
      attrs$legend_groups$borderWidth <- 1
    }
    
    legend_to_show <- c(5:10)[(attrs$legend_groups$label[5:10] %in% unique(df_nodes$Cap_label[!df_nodes$id %in% colnames(CosMatrix)]))]

    if(cluster){
      df_nodes$mass[1:length(root.node)]=40
      a = df_edges$length[df_edges$edgetype == "target-target"]
      df_edges$length[df_edges$edgetype == "target-target"] = sapply(a, function(x){max(x,300*min(10,length(root.node)))})
      p <- visNetwork::visNetwork(df_nodes, df_edges, width = "100%",height = "100%") %>%
        visNetwork::visLegend(width = 0.09, position = "right",
                  addNodes = attrs$legend_groups[c(1:4, legend_to_show, 18, 19), ],
                  addEdges = attrs$legend_edges,
                  useGroups = FALSE, zoom = TRUE,
                  stepX = 150, stepY = 75,ncol=1) %>%
        visNetwork::visClusteringByGroup(groups = df_groups$group,
                             label = "Group:\n",
                             scale_size = TRUE,
                             shape = "database",
                             color = df_groups$color.background,
                             force = TRUE)
      add_attr_network(p)
    }else{
      p <- visNetwork::visNetwork(df_nodes, df_edges, width = "100%",height = "100%") %>%
        visNetwork::visLegend(addNodes = attrs$legend_groups[c(1:4, legend_to_show, 19),],
                  addEdges = attrs$legend_edges,
                  width = 0.09,
                  position = "right",
                  useGroups = FALSE,
                  zoom = TRUE,
                  stepX = 150,
                  stepY = 70,
                  ncol=1)
      add_attr_network(p, layout)
    }

}
