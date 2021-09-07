


## Clicked node text ============================================================
clickedNodeText <- function(node_id,
                            edge_matrix, dict.combine){

  # node_name = dict.combine$Description[match(node_now,dict.combine$Variable)]

  # loc.node_now = match(node_id, colnames(edge_matrix))
  #
  # if(node_id %in% colnames(edge_matrix)){
  #   node_now_identity = "in the interested list"
  # } else {
  #   node_now_identity = "not in the interested list"
  # }
  HTML(

  paste0("<h3>",node_id, " </h3>",
   "<b>Description: </b>",dict.combine$Description[match(node_id,dict.combine$Variable)],
   "<br><b>Group: </b>", dict.combine$Capinfo[match(node_id,dict.combine$Variable)],
   "<br><b>Patient prevalence: </b>", round(dict.combine$marginal_pat_VA[match(node_id,dict.combine$Variable)]/12600000,4),
   "<br><b>Ave count per patient: </b>", round(dict.combine$marginal_freq_VA[match(node_id,dict.combine$Variable)]/
                                                dict.combine$marginal_pat_VA[match(node_id,dict.combine$Variable)],2))
  )



}




AddToCandidate <- function(s, x, node_now, CosMatrix, session, dict.combine){
  if(length(s)!=0 & length(node_now)!=0){
    edge.ma.now = CosMatrix
    loc.node_now = match(node_now, colnames(edge.ma.now))
    if(is.na(loc.node_now)==FALSE){
      node_now_name = colnames(edge.ma.now)[loc.node_now]
      if(!(node_now_name %in% x)){
        x = c(node_now_name, x)
        x.neighbor = sapply(x, function(xx){
          sum(CosMatrix[,xx]!=0)
        })
        x.name = dict.combine$Description[match(x,dict.combine$Variable)]
        x.neighbor = paste0(x.name," (" ,x.neighbor," degrees)")
        updateCheckboxGroupInput(session, "inCheckboxGroup2",
                                 label = paste(length(x), " candidate nodes:"),
                                 choiceValues = x,
                                 choiceNames = x.neighbor,
                                 selected = x
        )
      }
    }
  }
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
