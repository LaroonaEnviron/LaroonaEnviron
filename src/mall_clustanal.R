require(factoextra)
require(RColorBrewer)

# check standard mallee set
str(mall_sc)
head(mall_sc, 10)
summary(mall_sc)

# derive 7 kmean clusters and visualize
set.seed(1111)
mall_km <- kmeans(mall_sc, 7, nstart=50)
print(mall_km)

# display means from original data
round(aggregate(mallee[, -c(3, 6, 9, 10, 14:16)], by=list(cluster=mall_km$cluster), mean), 3)

# visualize clusters
fviz_cluster(mall_km, data = mall_sc,
  palette = "",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.2, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6
  )

mall_kmcp <- mall_km
mall_kmcp$cluster <- ifelse(mall_km$cluster == 3 | mall_km$cluster == 7, 7, mall_km$cluster)
mall_kmcp$cluster <- ifelse(mall_kmcp$cluster <= 2, mall_kmcp$cluster, mall_kmcp$cluster-1)
round(aggregate(mallee[, -c(3, 6, 9, 10, 14:16)], by=list(cluster=mall_kmcp$cluster), mean), 3)
print(mall_kmcp)

# visualize clusters
fviz_cluster(mall_kmcp, data = mall_sc, stand=FALSE, 
  palette = "",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.3, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6
  )

mallee$km1 <- mall_km$cluster
mallee$km2 <- mall_kmcp$cluster

