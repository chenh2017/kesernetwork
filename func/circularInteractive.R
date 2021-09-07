
## Generate interactive circular plot using highcharter ======================
circularInteractive <- function(thr_cos_pop,
                                node, edge_matrix, 
                                dict.combine, color.df){
  node_cos = edge_matrix[, match(node, colnames(edge_matrix)), drop = FALSE]
  summ = summary(node_cos)
  
  data = data.frame(ids = rownames(node_cos)[summ$i],
                    value = summ$x)
  data = left_join(data, dict.combine[, c("Variable", "Description", "Capinfo", "type")], 
                   by = c("ids" = "Variable")) 
  df = data[data$value>thr_cos_pop, ]
  
  # print(nrow(df))
  # print(head(df))
  
  if(nrow(df)>0){
    df$color = as.factor(color.df$color[match(df$Capinfo,color.df$name)])
    df$group_label = paste0("(",df$type,") ",df$Capinfo)
    df = df[order(df$type,df$Capinfo,df$value),]
    
    df$ids <- gsub("-", "_", df$ids, fixed = TRUE)
    
    df %>%
      hchart(type = "column", hcaes(x = ids, y = value, group=group_label), 
             color=levels(df$color)) %>%
      hc_plotOptions(column = list(stacking = "normal", borderWidth=0, gridLineWidth=0,
                                   pointPadding=0, groupPadding=0.1)) %>%
      hc_yAxis(tickInterval = 0.2, lineWith=1,
               title = list(text=""),
               showLastLabel=TRUE,
               max = max(df$value),
               reversedStacks = FALSE,
               startOnTick = FALSE,
               endOnTick = FALSE) %>%
      hc_xAxis(
        offset = -40,
        labels = list(
        allowOverlap=TRUE,
        step=1,
        align="left",
        staggerLines = 2,
        showLastLabel = TRUE
      ), title = list(text=""))%>%
      hc_legend(
        align = "right",
        verticalAlign = 'top',
        layout = 'vertical',
        x = 0,
        y = 100
      ) %>%
      hc_pane(size= '55%', innerSize='20%'#,
              # startAngle=10, endAngle=350
              ) %>%
      hc_chart(type = 'column', polar = TRUE, inverted = FALSE,
               height = 700, width = 800,
               events = list(
                 render = JS(paste0("function() {
                  var ticks = this.xAxis[0].ticks,
                      length = 0;
                      
                  Highcharts.objectEach(ticks, function(tick) {
                      length += 1;
                  });
                  var rotation = 90 / length - 90;
                      
                  var i = 1;
                  Highcharts.objectEach(ticks, function(tick) {
                      i += 1;
                      tick.label.attr({
                        	rotation: rotation
                      });
                      rotation += 360 / length;
                      /*if (i > length/2){
                          rotation -= 180;
                          i = 0;
                      }*/
                  });
               }"))
                 #   render = JS(paste0("function() {
                 #    var ticks = this.xAxis[0].ticks,
                 #        length = ", nrow(df), ",
                 #        rotation = 90 / length - 90;
                 #        alert(ticks.length);
                 #        
                 #        for (var i=0; i < length; i++){
                 #            ticks[i].label.attr({
                 #              	rotation: rotation
                 #            });
                 #            rotation += 360 / length;
                 #            if (i > (length/2)){
                 #                rotation -= 180;
                 #            }
                 #        }
                 #    });
                 # }"))
            )) %>% 
      hc_tooltip(
        formatter = JS("function() {
        var text = '<b>ID: </b>' + this.point.ids + '<br><b>cosine similarity: </b>' + this.y.toFixed(2) + '<br><b>Description: </b>' + this.point.Description + '<br><b>Group: </b>' + this.series.name;
        return text;
        }"
        )
      )
  }
  
  else{
    highchart() %>%
      hc_title(
        text = "After filtering, no connected node is left!",
        margin = 20,
        align = "center",
        style = list(color = "black", useHTML = TRUE)
      )
    
  }
}
