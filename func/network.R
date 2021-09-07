dataNetwork <- function(root.node, CosMatrix, dict.combine, attrs){

  attr_edges <- attrs$attr_edges
  attr_nodes_type <- attrs$attr_nodes_type
  attr_nodes_cap <- attrs$attr_nodes_cap

  cos.matrix = CosMatrix[ , root.node, drop = FALSE]
  print(nrow(cos.matrix))

  rootids = colnames(cos.matrix)

  summ <- summary(cos.matrix)
  df_edges <- data.frame(from = colnames(cos.matrix)[summ$j],
                         to = rownames(cos.matrix)[summ$i],
                         corvalue = summ$x)

  df_edges$ends <- paste0(df_edges$from, ";",df_edges$to)
  df_edges$ends <- sapply(df_edges$ends, function(x){
    paste(sort(strsplit(x, ";", fixed = T)[[1]]), collapse = ";")
  })

  df_edges <- df_edges[!duplicated(df_edges$ends), ]
  df_edges <- df_edges[, -4]

  df_edges$length <- abs(df_edges$corvalue)^(-1.1)*10
  df_edges$title <- paste0(df_edges$from,"<b> &rarr; </b>", df_edges$to)
  df_edges$edgetype <- "target-other"
  df_edges$edgetype[df_edges$from %in% rootids &
                      df_edges$to %in% rootids ] <- "target-target"


  df_edges <- left_join(df_edges, attr_edges, by = "edgetype")

  df_nodes <- data.frame(id = unique(c(df_edges$from, df_edges$to)))
  df_nodes <- left_join(df_nodes, dict.combine[, c(1, 5, 7, 8, 4)], by = c("id" = "Variable"))
  colnames(df_nodes) <- c("id", "label", "group", "group_title", "Cap")
  df_nodes$nodetype <- "other"
  df_nodes$nodetype[df_nodes$id %in% rootids] <- "target"



  df_nodes <- left_join(df_nodes, attr_nodes_type, by = "nodetype")
  df_nodes <- left_join(df_nodes, attr_nodes_cap, by = "Cap")

  df_nodes$group[df_nodes$id %in% rootids] <- df_nodes$label[df_nodes$id %in% rootids]
  df_nodes$Cap_label[df_nodes$id %in% rootids] <- "target"
  df_nodes$color.highlight.border[df_nodes$id %in% rootids] <- "#FF0000"
  df_nodes$color.hover.border[df_nodes$id %in% rootids] <- "#FF0000"


  df_nodes$title = paste0("<b>ID: </b>",df_nodes$id,
                          "<br><b>Description: </b>",dict.combine$Description[match(df_nodes$id,dict.combine$Variable)],
                          "<br><b>Group: </b>", df_nodes$group_title,
                          "<br><b>Patient prevalence: </b>", round(dict.combine$marginal_pat_VA[match(df_nodes$id,dict.combine$Variable)]/12600000,4),
                          "<br><b>Ave count per patient: </b>", round(dict.combine$marginal_freq_VA[match(df_nodes$id,dict.combine$Variable)]/
                                                             dict.combine$marginal_pat_VA[match(df_nodes$id,dict.combine$Variable)],2)
  )

  df_nodes$font.background[is.na(df_nodes$font.background)] <- ""


  df_nodes$font.size = 30
  nchar40 = nchar(df_nodes$label)>40
  df_nodes$font.size[nchar40] = ((df_nodes$font.size[nchar40])*40)/(nchar(df_nodes$label[nchar40]))
  df_nodes$font.size[df_nodes$id %in% rootids] = 35
  maxcorvalue = sapply(1:nrow(df_nodes), function(i){max(df_edges$corvalue[(df_nodes$id[i]==df_edges$from) | (df_nodes$id[i]==df_edges$to)])})
  df_nodes$font.size = df_nodes$font.size*sapply(maxcorvalue, function(x){
    min(max(1,x*5), 3)
  })

  df_groups = df_nodes[, c(3, 16)]
  df_groups <- df_groups[!duplicated(df_groups),]

  return(list(df_edges, df_nodes, df_groups))
}


add_attr_network <- function(p){
  p %>% visNodes(color = list(background = "lightblue",
                              border = "darkblue",
                              highlight = "yellow"),
                 shadow = list(enabled = TRUE, size = 10)) %>%
    visEdges(physics = FALSE,
             smooth = FALSE,
             hoverWidth = 2.5) %>%
    visOptions(highlightNearest = list(enabled = T,
                                       degree = 1,
                                       hover = FALSE,
                                       hideColor = "rgba(200,200,200,0.2)"),
               selectedBy = list(`variable` = "Cap_label",
                                 `multiple` = TRUE,
                                 `main` = "Select by group"),
               collapse = FALSE) %>%
    visInteraction(hover = TRUE,
                   navigationButtons = TRUE) %>%
    visIgraphLayout(layout = "layout_nicely",
                    physics = FALSE,
                    smooth = FALSE,
                    type = "square") %>%
    visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                    ;}") %>%
    visLayout(randomSeed = 10) # to have always the same network
}


plot_network <- function(s, cluster, draw.data, hide_label, CosMatrix, dict.combine, attrs){

    input.correct = s[1:min(50,length(s))]
    root.node = match(input.correct, colnames(CosMatrix))
    df_edges = draw.data[[1]]
    df_nodes = draw.data[[2]]
    df_groups = draw.data[[3]]
    attrs$legend_groups <- attrs$legend_groups[attrs$legend_groups$label %in% c(unique(df_nodes$Cap_label), "Node:", "Group:"), ]
    if(hide_label){
      df_nodes$label[df_nodes$nodetype == "other"] <- ""
      df_nodes$font.background <- NA
      df_nodes$shape[df_nodes$nodetype == "other"] <- "circle"
      attrs$legend_groups$shape[1:7] <- "dot"
      attrs$legend_groups$size[1:7] <- 10
      attrs$legend_groups$borderWidth <- 0
      }
    if(cluster){
      df_nodes$mass[1:length(root.node)]=40
      a = df_edges$length[df_edges$edgetype == "target-target"]
      df_edges$length[df_edges$edgetype == "target-target"] = sapply(a, function(x){max(x,300*min(10,length(root.node)))})
      p <- visNetwork(df_nodes, df_edges, width = "100%",height = "100%") %>%
        visLegend(width = 0.09, position = "right",
                  addNodes = attrs$legend_groups,
                  addEdges = attrs$legend_edges,
                  useGroups = FALSE, zoom = FALSE,
                  stepX = 150, stepY = 75,ncol=1) %>%
        visClusteringByGroup(groups = df_groups$group,
                             label = "Group:\n",
                             scale_size = TRUE,
                             shape = "ellipse",
                             color = df_groups$color.background,
                             force = TRUE)
      add_attr_network(p)
    }else{
      p <- visNetwork(df_nodes, df_edges, width = "100%",height = "100%") %>%
        visLegend(addNodes = attrs$legend_groups[1:(nrow(attrs$legend_groups)/2),],
                  addEdges = attrs$legend_edges,
                  width = 0.09,
                  position = "right",
                  useGroups = FALSE,
                  zoom = FALSE,
                  stepX = 150,
                  stepY = 75,
                  ncol=1)
      add_attr_network(p)
    }

}
