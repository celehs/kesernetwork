


## Clicked node text ============================================================
clickedNodeText <- function(node_id, dict.combine){
  
  title = paste0("<h3>",node_id, " </h3>",
                 "<br><b>Description: </b>",dict.combine$Description[match(node_id,dict.combine$Variable)],
                 "<br><b>Group: </b>", dict.combine$group[match(node_id,dict.combine$Variable)])
  if(!is.na(dict.combine$marginal_pat_VA[match(node_id,dict.combine$Variable)])){
    title = paste0(title,
                   "<br><b>Patient prevalence: </b>", 
                   round(dict.combine$marginal_pat_VA[match(node_id,dict.combine$Variable)]/12600000,4))
  }
  if(!is.na(dict.combine$marginal_freq_VA[match(node_id,dict.combine$Variable)]/
            dict.combine$marginal_pat_VA[match(node_id,dict.combine$Variable)])){
    title = paste0(title,
                   "<br><b>Ave count per patient: </b>", 
                   round(dict.combine$marginal_freq_VA[match(node_id,dict.combine$Variable)]/
                         dict.combine$marginal_pat_VA[match(node_id,dict.combine$Variable)],2))
  }

  HTML(title)
}


updateCheckboxCandidate <- function(x, CosMatrix, session, dict.combine){
  if (length(x) != 0) {
    x.name <- dict.combine$Description[match(x, dict.combine$Variable)]
    x.neighbor <- sapply(x, function(xx) length(getNeighbors(xx, CosMatrix())))
    x.neighbor <- paste0(x.name, " (", x.neighbor, " neighbors)")
  } else {
    x <- x.name <- x.neighbor <- character(0)
  }
  updateCheckboxGroupInput(session, "inCheckboxGroup2",
                           label = paste(length(x), " nodes selected:"),
                           choiceValues = x,
                           choiceNames = x.neighbor,
                           selected = x
  )
}


WriteData <- function(s, draw.data){
  downloadHandler(
    filename = "node.csv",
    content = function(path) {
      if(length(s)!=0){
        input.correct = s[seq(1,min(50,length(s)),by=1)]
        edges = draw.data[[1]]
        nodes = draw.data[[2]]
        file = edges[,c(1,2,3,6)]
        file$from.term = nodes$label[match(file$from,nodes$id)]
        file$to.term = nodes$label[match(file$to,nodes$id)]
        file = file[,c(4,1,5,2,6,3)]
        file = file[order(file$corvalue,decreasing = TRUE),]
      }else{
        file = data.frame("Warning"="Try to click some rows in the 'Possible inputs' box to specify your nodes!")
      }
      write.csv(file,path,row.names = FALSE)
    }
  )
}


getNeighbors <- function(node, cosmatrix){
  if(node %in% colnames(cosmatrix)){
    cosmatrix <- t(cosmatrix)
  }
  neighbors <- cosmatrix[node, ]
  names(neighbors)[neighbors != 0]
}


box_info <- function(title, info, height = 500, border_color = "#EEEEEE"){
  div(
    p(tags$b(title, style = "padding-left: 5px;"),
      style = "margin-top: 5px;"),
    div(info,
        style = paste0("height: ", height - 45, "px;
                        overflow: auto;
                        background: white;
                        margin-top: 5px;")
    ), style = paste0("height: ", height, "px;
                       box-shadow: #868585 0px 0px 5px;
                       background: ", border_color, ";
                       padding: 5px;")
  )
}

