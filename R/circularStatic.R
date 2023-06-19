
#' @importFrom dplyr %>% left_join
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom graphics title
#' @importFrom stats end na.omit start

## Generate circular plot using ggplot =======================================
circularPreData <- function(data){
  # Set a number of 'empty bar' to add at the end of each category
  empty_bar=3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$category), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$category=rep(levels(data$category), each=empty_bar)
  data=as.data.frame(rbind(data, to_add))
  data=data[order(data$category), ]
  data$id=seq(1, nrow(data))

  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data=data %>%
    dplyr::group_by(.data$category) %>%
    dplyr::summarize(start=min(.data$id), end=max(.data$id) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title=mean(c(.data$start, .data$end)))
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  if(nrow(grid_data)>1){
    grid_data=grid_data[-1,]
  }else{
    grid_data$start = -1
  }
  return(list(`data` = data, `label_data` = label_data, 
              `grid_data` = grid_data, `base_data` = base_data))
}

circularBar <- function(thr_cos_pop,
                        node_now, df_edges, 
                        dict.combine, attrs){
  
  data <- right_join(dict.combine[, c("id", "term", "category")], df_edges[, c("to", "weight")], by = c("id" = "to"))
  data = data %>% dplyr::arrange(.data$category, .data$weight)
  if(nrow(data)>0){
    data$weight = data$weight*100
    data$category = factor(data$category)
    
    circularData = circularPreData(data)
    data = circularData$data
    label_data = circularData$label_data
    grid_data = circularData$grid_data
    base_data = circularData$base_data
    
    
    # Make the plot
    p = ggplot(data, aes(x=as.factor(.data$id), y=.data$weight, fill=.data$category)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      # scale_fill_manual(weights = color.df$color) +
      geom_bar(aes(x=as.factor(.data$id), y=.data$weight, fill=.data$category), stat="identity") +
      
      # Add a val=.8/.6/.4/.2 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data=grid_data, aes(x = .data$end, y = 80, xend = .data$start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = .data$end, y = 60, xend = .data$start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = .data$end, y = 40, xend = .data$start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = .data$end, y = 20, xend = .data$start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      # Add text showing the weight of each .8/.6/.4/.2 lines
      annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("0.2", "0.4", "0.6", "0.8") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
      
      # geom_bar(aes(x=as.factor(id), y=weight, fill=category), stat="identity", alpha=0.5) +
      ylim(-50,max(na.omit(data$weight))+10) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(1,4), "cm")
      ) +
      coord_polar() +
      geom_text(data=label_data, aes(x=.data$id, y=.data$weight+3, label=.data$term, hjust=.data$hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
      #
      # Add base line information
      geom_segment(data=base_data, aes(x = .data$start, y = -5, xend = .data$end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = .data$title, y = -10, label=.data$category), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE) + 
      scale_fill_manual(values=colorList(attrs))
    p
  }else{
    plot(1:10,1:10,xaxt="n",yaxt="n",bty="n",type="n",xlab="",ylab="",
         main = "After filtering, no connected node is left!")
  }
}


colorList <- function(attrs){
  list_color <- attrs$cap_color$color
  names(list_color) <- attrs$cap_color$index01
  return(list_color)
}
