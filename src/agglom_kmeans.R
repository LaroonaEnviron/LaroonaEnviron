require(factoextra)

# derive kmeans
mallee.km01 <- eclust(mall_sc, "kmeans", k=7, seed=123)
names(mallee.km01)
fviz_cluster(mallee.km01,  ellipse.type = "convex", labelsize=10, ggtheme=theme_minimal())

# Visualize the silhouette of clusters
fviz_silhouette(mallee.km01)

# derive hierarchical clusters
mallee.hc01 <- eclust(mall_sc, "hclust", k = 7, method = "ward.D2", graph = TRUE)

# Dendrogram
fviz_dend(mallee.hc01, rect = TRUE, show_labels = TRUE, cex = 0.5)
names(mallee.hc01) 

# Visualize the silhouette of clusters
fviz_silhouette(mallee.hc01)

# extract hierarchical groups
mallee.hgrps <- mallee.hc01$cluster

 # Compute cluster centers
clus.centers <- aggregate(mall_sc, list(mallee.hgrps), mean)
clus.centers

# Remove the first column
clus.centers <- clus.centers[, -1]
clus.centers

# K-means clustering using hierarchical clustering defined cluster-centers
mallee.km02 <- eclust(mall_sc, "kmeans", k = clus.centers, graph = TRUE)
fviz_cluster(mallee.km02,  ellipse.type = "convex", labelsize=10, ggtheme=theme_minimal())
fviz_silhouette(mallee.km02)

mallee.km01$withinss
mallee.km02$withinss

mallee.km01$centers
mallee.km02$centers

fviz_cluster(mallee.km02,  choose.vars=c("Clay","Carbonate"), ellipse.type = "convex", labelsize=10, ggtheme=theme_minimal())

# use hkmenas package
mallee.hk <- hkmeans(mall_sc, 7)
mallee.hk$withinss # print results

# Visualize the tree
fviz_dend(mallee.hk, cex = 0.6, palette = "jco",
    rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# visualize clusters
fviz_cluster(list(data=mall_sc, cluster=mallee.hk$cluster), 
  palette = "uchicago",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.3, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6,
  main="Heirarchical Cluster - kmeans"
  )
  
str(mallee)
mallee$hkclust <- mallee.hk$cluster
mallee.cntrs01 <- aggregate(mallee[, 1:13], list(mallee$hkclust), mean)
mallee.cntrs02 <- aggregate(mallee[, 1:13], list(mallee$rawclass), mean)

with(mallee, tapply(raw60, list(Field.CO3, hkclust), length))
with(mallee, tapply(raw60, list(Field.CO3, rawclass), length))
with(mallee, tapply(raw60, list(Field.CO3, hkclust), length))
with(mallee, tapply(Field.Texture, list(Field.Texture, hkclust), length))
