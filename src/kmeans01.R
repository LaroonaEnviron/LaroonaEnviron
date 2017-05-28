str(soils)

# scale soils to standardize
soils.sc <- scale(soils[, 1:11])
head(round(soils.sc, 3), 10)

# The R function fviz_nbclust() [in factoextra package] provides a convenient solution
# to estimate the optimal number of clusters.

require(factoextra)
require(cluster)

fviz_nbclust(soils.sc, kmeans, method = "wss") + geom_vline(xintercept = 6, linetype = 2)

# The R code below performs k-means clustering with k = 6: Compute k-means with k = 6

set.seed(1234)
soils_km <- kmeans(soils.sc, 6, nstart=25)
print(soils_km)

#It’s possible to compute the mean of each variables by clusters using the original data:
aggregate(soils[, 1:11], by=list(cluster=soils_km$cluster), mean)

#Visualizing k-means clusters
# if we have a multi-dimensional data set, a solution is to perform
#Principal Component Analysis (PCA) and to plot data points according to the first
#two principal components coordinates

fviz_cluster(soils_km, data = soils.dat,  
  palette = c("red", "orange", "brown", "green", "blue", "violet"),
  ellipse.type = "t", # Concentration ellipse
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  main="k-Means Clustering", # Main title
  ggtheme = theme_bw()
  )
