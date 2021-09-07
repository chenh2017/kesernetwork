
## Generate sunburst plot using plotly =======================================
sunburstDF <- function(DF, valueCol = NULL, root.name = "Root"){
  
  # root.name <- node_id
  # valueCol <- "corvalue"
  
  colNamesDF <- names(DF)
  
  if(data.table::is.data.table(DF)){
    DT <- data.table::copy(DF)
  } else {
    DT <- data.table::data.table(DF, stringsAsFactors = FALSE)
  }
  
  DT[, root := root.name]
  colNamesDT <- names(DT)
  
  if(is.null(valueCol)){
    data.table::setcolorder(DT, c("root", colNamesDF))
  } else {
    data.table::setnames(DT, valueCol, "values", skip_absent=TRUE)
    data.table::setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
  }
  
  hierarchyCols <- setdiff(colNamesDT, "values")
  hierarchyList <- list()
  
  for(i in seq_along(hierarchyCols)){
    currentCols <- colNamesDT[1:i]
    if(is.null(valueCol)){
      currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
    }
    #currentDT = stats::na.omit(currentDT)
    data.table::setnames(currentDT, length(currentCols), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- data.table::rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
  hierarchyDT[, parents := apply(.SD, 1, function(x){data.table::fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parentCols) := NULL]
  return(hierarchyDT)
}
sunburstPreData <- function(df, changeline){
  
  # df <- hierarchyDT
  # changeline <- 10
  
  df = df[!is.na(df$labels), ]
  df$labels = stringr::str_replace(df$labels, "^.*_Codified","Codified")
  df$labels = stringr::str_replace(df$labels, "^.*_NLP","NLP")
  df$labels = stringr::str_replace(df$labels, "Ignore_cui","Others")
  df$labels = stringr::str_replace(df$labels, ",...",", ...")
  df$text = df$labels
  label = df$labels[stringr::str_length(df$labels)>5]
  label_split = stringr::str_split(label," ")
  if(changeline != 99){
    label_com = sapply(label_split, function(x){
      y = ""
      i = 1
      k = 0
      while(i <= length(x)){
        y = paste(y, x[i])
        k = k + stringr::str_length(x[i])
        if(k>=changeline & i!=length(x)){
          y = paste0(y,"<br>")
          k = 0
        }
        i = i + 1
      }
      return(stringr::str_trim(y,side = "both"))
    })
    df$text[stringr::str_length(df$labels)>5] = label_com
  }
  return(df)
}

sunburstPlot <- function(thr_cos,
                           changeline,rotatelabel,scale_sungh,
                           node_id, CosMatrix, 
                           dict.combine, cap_color){
  
  # node_id <- "PheCode:008.5"
  # CosMatrix <- cos.list[[1]]
  # thr_cos <- 0.01
  # scale_sungh <- 750
  
  node_name = dict.combine$Description[match(node_id,dict.combine$Variable)]
  
  node_loc = match(node_id, colnames(CosMatrix))
  if(!is.na(node_loc)){
    if(length(node_name)>0 & !is.na(node_name)){
      
      data = CosMatrix[, node_loc, drop = FALSE]
      
      summ <- summary(data)
      rhd <- data.frame(id = rownames(data)[summ$i],
                        corvalue = summ$x)
      
      rhd <- rhd[rhd$corvalue > thr_cos, ]
      
      rhd <- left_join(rhd, dict.combine[, c("Variable","Capinfo","index01","index1","index2","index3","index4")],
                       by = c("id" = "Variable"))
      
      if(nrow(rhd)>0){
        rhd = rhd[order(rhd$index01,rhd$index1,rhd$index2,
                        rhd$index3,rhd$index4,rhd$corvalue),]
        DF = rhd[,-c(1,3)]
        df = sunburstDF(DF,valueCol = "corvalue",root.name = node_name)
        df = sunburstPreData(df, changeline)
        
        df$parents2 = sapply(df$ids, function(x)(strsplit(x, " - ", fixed = T)[[1]][2]))
        
        
        # df.color <- data.frame(index01 = c("Disease", "Drug", "Lab", "Procedure"),
        #                        color = c("#1F78B4", "#33A02C", "#FF7F00", "#E31A1C"))
        df = left_join(df, cap_color[, c(3,2)], by = c("parents2" = "index01"))
        df$color[is.na(df$color)] <- "white"
        
        print(nrow(df))

        m <- list(
          l = 0,r = 0,b = 0,t = 0,pad = 0
        )
        
        if(rotatelabel=="Radial"){
          plotly::plot_ly(data = df, ids = ~ids, labels= ~labels, parents = ~parents, 
                          text = ~text, values= ~values, type='sunburst', branchvalues = 'total',
                          hoverinfo = "label", textinfo = "text", textfont = list(color="black"),
                          insidetextorientation='radial',
                          width = scale_sungh,
                          marker = list(colors = df$color),
                          height =  scale_sungh)%>%
            plotly::layout(autosize = F, margin = m)
        }else{
          plotly::plot_ly(data = df, ids = ~ids, labels= ~labels, parents = ~parents, 
                          text = ~text, values= ~values, type='sunburst', branchvalues = 'total',
                          hoverinfo = "label", textinfo = "text", textfont = list(color="black"),
                          insidetextorientation='"tangential"',
                          width = scale_sungh,
                          marker = list(colors = df$color),
                          height =  scale_sungh)%>%
            plotly::layout(autosize = F, margin = m)
        }
        
      }else{
        plotly::plot_ly(data = data.frame(ids=c(),labels=c(),parents=c(),text=c(),
                                          values=c()),type='sunburst')%>%
          plotly::layout(title = "After filtering, no connected node is left!")
      }
    }
  }else{
    plotly::plot_ly(data = data.frame(ids=c(),labels=c(),parents=c(),text=c(),
                                      values=c()),type='sunburst')%>%
      plotly::layout(title = "The node you click is not on the interested list!")
  }
}
