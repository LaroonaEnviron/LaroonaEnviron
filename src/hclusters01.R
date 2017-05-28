# heirarchical clustering

require(factoextra)
require(cluster)

# Compute the dissimilarity matrix
# df = the standardized data
soils.dist <- dist(soils.sc, method = "euclidean")

# The R code below displays the first 6 rows and columns of the distance matrix:
round(as.matrix(soils.dist)[1:10, 1:10], 3)

# hclust() can be used as follow:

soils.hc <- hclust(d = soils.dist, method = "ward.D2")

# Here, we’ll use the function fviz_dend()[ in factoextra R package] to produce 
# a beautiful dendrogram.
# cex: label size

fviz_dend(soils.hc, cex = 0.6, ggtheme=theme_bw())

# 7.3 Verify the cluster tree
# The R base function cophenetic() can be used to compute the cophenetic distances for
# hierarchical clustering.
# Compute cophentic distance

soils.coph <- cophenetic(soils.hc)

# Correlation between cophenetic distance and  the original distance

cor(soils.dist, soils.coph)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Use method = complete
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
soils.hc01 <- hclust(d = soils.dist, method = "complete")

# Here, we’ll use the function fviz_dend()[ in factoextra R package] to produce 
# a beautiful dendrogram.
# cex: label size

fviz_dend(soils.hc01, cex = 0.6, ggtheme=theme_bw())

# 7.3 Verify the cluster tree
# The R base function cophenetic() can be used to compute the cophenetic distances for
# hierarchical clustering.
# Compute cophentic distance

soils.coph <- cophenetic(soils.hc01)

# Correlation between cophenetic distance and  the original distance

cor(soils.dist, soils.coph)

#=====================================================================
# use method = average
#=====================================================================
soils.hc02 <- hclust(d = soils.dist, method = "average")

# Here, we’ll use the function fviz_dend()[ in factoextra R package] to produce 
# a beautiful dendrogram.
# cex: label size

fviz_dend(soils.hc02, cex = 0.6, ggtheme=theme_bw())

# 7.3 Verify the cluster tree
# The R base function cophenetic() can be used to compute the cophenetic distances for
# hierarchical clustering.
# Compute cophentic distance

soils.coph <- cophenetic(soils.hc02)

# Correlation between cophenetic distance and  the original distance

cor(soils.dist, soils.coph)

#------------------------------------------------------------------------

# Cut tree into 6 groups
soils.hcgrp <- cutree(soils.hc, k = 6)
head(soils.hcgrp, n = 10)

# Number of members in each cluster
table(soils.hcgrp)

# Get the names for the members of cluster 1
rownames(soils.sc)[soils.hcgrp == 1]

# visualize the groups
# Cut in 6 groups and color by groups

fviz_dend(soils.hc, k = 6, # Cut in six groups
cex = 0.4, # label size
k_colors = c("red", "orange", "green", "brown", "blue", "purple"),
color_labels_by_k = TRUE, # color labels by groups
rect = TRUE, rect_border="lightgrey", # Add rectangle around groups
main="Mallee and Barossa Soil Groups" , ggtheme=theme_bw()
)

# visualize on scatter plot

fviz_cluster(list(data = soils.sc, cluster = soils.hcgrp),
palette = c("red", "orange", "green", "brown", "blue", "purple"),
ellipse.type = "t", # Concentration ellipse
ellipse.level=0.9, # confidence level
repel = TRUE, # Avoid label overplotting (slow)
main="Heirarchical Cluster Groups of Mallee and Barossa Soils",
labelsize = 6,
show.clust.cent = FALSE, ggtheme = theme_minimal())

require("cluster")
# Agglomerative Nesting (Hierarchical Clustering)
soils.agnes <- agnes(x = soils[, 1:11], # data matrix
          stand = TRUE, # Standardize the data
          metric = "euclidean", # metric for distance matrix
          method = "ward" # Linkage method
          )
          
# DIvisive ANAlysis Clustering
soils.diana <- diana(x = soils[, 1:11], # data matrix
          stand = TRUE, # standardize the data
          metric = "euclidean" # metric for distance matrix
          )

# After running agnes() and diana(), you can use the function fviz_dend()[in factoextra]
# to visualize the output:

fviz_dend(soils.agnes, cex = 0.6, k = 6, rect=TRUE, rect_border="darkgrey")
fviz_dend(soils.diana, cex = 0.6, k = 6, rect=TRUE, rect_border="grey")

soils.hcgrp01 <- cutree(soils.agnes, k = 6)
head(soils.hcgrp01, n = 10)

fviz_cluster(list(data = soils.sc, cluster = soils.hcgrp01),
palette = c("red", "orange", "green", "brown", "blue", "purple"),
ellipse.type = "convex", # Concentration ellipse
ellipse.level=0.9, # confidence level
repel = TRUE, # Avoid label overplotting (slow)
main="AGNES Cluster Groups of Mallee and Barossa Soils",
labelsize = 6,
show.clust.cent = FALSE, ggtheme = theme_bw())

soils.hcgrp02 <- cutree(soils.diana, k = 6)
head(soils.hcgrp02, n = 10)

fviz_cluster(list(data = soils.sc, cluster = soils.hcgrp02),
palette = c("red", "orange", "green", "brown", "blue", "purple"),
ellipse.type = "convex", # Concentration ellipse
ellipse.level=0.9, # confidence level
repel = TRUE, # Avoid label overplotting (slow)
main="DIANA Cluster Groups of Mallee and Barossa Soils",
labelsize = 6,
show.clust.cent = FALSE, ggtheme = theme_bw())

# Comparing dendrograms
require(dendextend)

# Compute 2 hierarchical clusterings
soils.hc1 <- hclust(soils.dist, method = "average")
soils.hc2 <- hclust(soils.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (soils.hc1)
dend2 <- as.dendrogram (soils.hc2)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)

# To visually compare two dendrograms, we’ll use the tanglegram() function [dendextend
# package]
# Draw a tanglegram:

tanglegram(dend1, dend2)

# Customized the tanglegram using many other options as follow:

tanglegram(dend1, dend2,
                  highlight_distinct_edges = FALSE, # Turn-off dashed lines
                  common_subtrees_color_lines = FALSE, # Turn-off line colors
                  common_subtrees_color_branches = TRUE, # Color common branches
                  main = paste("entanglement =", round(entanglement(dend_list), 2))
                  )

# The function cor.dendlist() is used to compute “Baker” or “Cophenetic” correlation
# matrix between a list of trees

# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic")

# Baker correlation matrix
cor.dendlist(dend_list, method = "baker")

# The correlation between two trees can be also computed as follow:
# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2)

# Baker correlation coefficient
cor_bakers_gamma(dend1, dend2)

# It’s also possible to compare simultaneously multiple dendrograms. A chaining operator
# %>% is used to run multiple function at the same time. It’s useful for simplifying the
# code:
# Create multiple dendrograms by chaining
dend1 <- soils.sc %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- soils.sc %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- soils.sc %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- soils.sc %>% dist %>% hclust("centroid") %>% as.dendrogram

# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2, "Average" = dend3, "Centroid" = dend4)
cors <- cor.dendlist(dend_list)

# Print correlation matrix
round(cors, 2)

# Visualize the correlation matrix using corrplot package
require(corrplot)
corrplot(cors, "pie", "lower")
