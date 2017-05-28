require(factoextra)
require(cluster)

# calculate the dissimilarity matrix using euclidean distance (get_dist() default)
mall.dist <- get_dist(mall_sc)
as.matrix(mall.dist)[1:6, 1:6]

# visualize distance matrix
fviz_dist(mall.dist, order = TRUE, show_labels = TRUE, lab_size = NULL,
  gradient = list(low = "red", mid = "white", high = "blue"))

# derive heirarchal cluster - agglomerative  and visuzlize
mall.hc <- hclust(d=mall.dist, method="ward.D2")
fviz_dend(mall.hc, k=7, k_colors="uchicago", 
     type="rectangle", 
     rect = TRUE,
     rect_border = "uchicago", rect.lty = 2,
     rect_fill=TRUE, 
     main = "Heirarchical cluster - ward.D2",
     ggtheme=theme_minimal(),
     lwd=0.75, cex = 0.4)

# derive heirarchal cluster - Complete")
mall.hc01 <- hclust(d=mall.dist, method="complete")
fviz_dend(mall.hc01, k=7, k_colors="uchicago", 
     type="rectangle", 
     rect = TRUE,
     rect_border = "uchicago", rect.lty = 2, 
     main = "Heirarchical cluster - Complete",
     ggtheme=theme_minimal(),
     lwd=0.75, cex = 0.4)

# Compute cophentic distance - waerd.D2
mall.coph <- cophenetic(mall.hc)

# Correlation between cophenetic distance and
# the original distance
cor(mall.dist, mall.coph)

# Compute cophentic distance - complete
mall.coph01 <- cophenetic(mall.hc01)

# Correlation between cophenetic distance and
# the original distance
cor(mall.dist, mall.coph01)
cor(mall.coph, mall.coph01)

# cut tree into 7 groups
mall.grp <- cutree(mall.hc, k=7)
mall.grp01 <- cutree(mall.hc01, k=7)

mall.grp <- ifelse(mall.grp==2 | mall.grp==5, 5, mall.grp)
mall.grp <- ifelse(mall.grp > 1, mall.grp-1, mall.grp)

table(mall.grp)
table(mall.grp01)

mallee$ward <- mall.grp
str(mallee)

# visualize clusters
fviz_cluster(list(data=mall_sc, cluster=mall.grp), 
  palette = "uchicago",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.3, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6,
  main="Heirarchical Cluster - Ward.D2"
  )

fviz_cluster(list(data=mall_sc, cluster=mall.grp01), 
  palette = "",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.3, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6,
  main="Heirarchical Cluster - Complete"
  )

# Agglomerative Nesting (Hierarchical Clustering) 
mall.agnes <- agnes(x=mall_sc, stand=FALSE, method="ward")

# DIvisive ANAlysis Clustering
mall.diana <- diana(x = mall_sc, # data matrix
    stand = FALSE, # standardize the data
    metric = "euclidean" # metric for distance matrix
    )

fviz_dend(mall.agnes, k=7, k_colors="uchicago", 
     type="rectangle", 
     rect = TRUE,
     rect_border = "uchicago", rect.lty = 2, rect_fill=TRUE, 
     main = "Heirarchical cluster - ward.D2",
     ggtheme=theme_minimal(),
     lwd=0.75, cex = 0.4
     )
     
fviz_dend(mall.diana, k=7, k_colors="uchicago", 
     type="rectangle", 
     rect = TRUE,
     rect_border = "black", rect.lty = 2, rect_fill=TRUE, 
     main = "Heirarchical cluster - ward.D2",
     ggtheme=theme_minimal(),
     lwd=0.75, cex = 0.4
     )

# Create two dendrograms
require(dendextend)
dend1 <- as.dendrogram (mall.agnes)
dend2 <- as.dendrogram (mall.hc)
dend3 <- as.dendrogram (mall.hc01)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2, dend3)

tanglegram(dend2, dend3,
    highlight_distinct_edges = FALSE, # Turn-off dashed lines
    common_subtrees_color_lines = FALSE, # Turn-off line colors
    common_subtrees_color_branches = TRUE, # Color common branches
    main = paste("entanglement =", round(entanglement(dend_list), 2))
    )

# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic")

# Baker correlation matrix
cor.dendlist(dend_list, method = "baker")
