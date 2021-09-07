


library(plotly)


node <- "PheCode:008.5"
CosMatrix <- cos.list[[1]]
net_nodes <- CosMatrix[, match(node, colnames(CosMatrix)), drop = FALSE]
df <- summary(net_nodes)

df$ids <- rownames(net_nodes)[df$i]
df$cos <- df$x

df <- left_join(df[, 4:5], dict.combine[, c(1,2,19)], by = c("ids" = "Variable"))

df <- left_join(df, attrs$cap_color[, 1:2], by = c("Capinfo" = "name"))
df <- df[order(df$Capinfo,df$cos),]
df$x <- 1:nrow(df)
df$theta <- df$x/nrow(df) * 360


plot_ly(df, theta = ~ids, r = ~cos, type = 'barpolar',
        text = ~Description, 
        ids = ~ids,
        hovertemplate = "ID: %{id}\nDescription: %{text}\ncosine similarity: %{r}\nGroup: %{meta}",
        marker = list(color = df$color),
        meta = ~Capinfo,
        showlegend = TRUE)
  


df1 <- df[df$Capinfo == "CCS",]
df2 <- df[df$Capinfo == "Lab",]
df3 <- df[df$Capinfo == "PheCode",]
df4 <- df[df$Capinfo == "RXNORM",]


plot_ly()%>%
  add_trace(theta = df1$ids, r = df1$cos, text = df1$Description, type = 'barpolar',
            ids = df1$ids,
            name = "CCS",
            hovertemplate = "ID: %{id}\nDescription: %{text}\ncosine similarity: %{r}\nGroup: CCS",
            marker = list(color = df1$color))%>%
  add_trace(theta = df2$ids, r = df2$cos, text = df2$Description, type = 'barpolar',
            ids = df2$ids,
            name = "Lab",
            hovertemplate = "ID: %{id}\nDescription: %{text}\ncosine similarity: %{r}\nGroup: Lab",
            marker = list(color = df2$color))%>%
  add_trace(theta = df3$ids, r = df3$cos, text = df3$Description, type = 'barpolar', 
            ids = df3$ids,
            name = "PheCode",
            hovertemplate = "ID: %{id}\nDescription: %{text}\ncosine similarity: %{r}\nGroup: PheCode",
            marker = list(color = df3$color))%>%
  add_trace(theta = df4$ids, r = df4$cos, text = df4$Description, type = 'barpolar', 
            ids = df4$ids,
            name = "RXNORM",
            hovertemplate = "ID: %{id}\nDescription: %{text}\ncosine similarity: %{r}\nGroup: RXNORM",
            marker = list(color = df4$color))
# %>%
  # add_trace(
  #   r = df$cos + 1, 
  #   theta = df$ids,
  #   mode = "bar", type = 'scatterpolar',
  #   text = df$ids,
  #   
  #   textfont = list(color = '#000000', size = 12)
  # ) 


# text = ~Description, 
# ids = ~ids,
# hovertemplate = "ID: %{id}\nDescription: %{text}\ncosine similarity: %{r}\nGroup: %{meta}",
# marker = list(color = df$color),
# meta = ~Capinfo,
# showlegend = TRUE)


