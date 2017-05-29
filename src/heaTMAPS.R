heatmap(as.matrix(mall_sc), scale="none", cexCol=2, cexRow=0.5)

require("RColorBrewer")
col <- colorRampPalette(brewer.pal(12, "Set3"))(301)
heatmap(as.matrix(mall_sc), scale = "row", col = col, cexRow=1, 
            RowSideColors = c(rep("blue", 100), rep("green", 101), rep("cyan", 100)),
            ColSideColors = c(rep("yellow", 3), rep("orange", 3), rep("blue", 3)))

require(clValid)

# list methods
clmethods <- c("kmeans", "pam", "hierarchical", "diana", "model", "sota") 
internal <- clValid(mall_sc, nClust=5:8, clMethods=clmethods, validation="internal", method="ward") 
summary(internal)

# stability measures
stable <- clValid(mall_sc, nClust=5:9, clMethods=clmethods, validation=c("stability", "internal"), method="complete") 
summary(stable)

sotaCL <- sota(as.matrix(mall_sc), 5) 
sotaCL
plot(sotaCL) 
sotaCL$clust

require(factoextra)
fviz_cluster(list(data=mall_sc, cluster=sotaCL$clust),
  palette = "",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.2, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6
  )
