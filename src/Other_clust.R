require(cluster)
require(factoextra)

# fuzzy clustering
mall.fuzzy <- fanny(mall_sc, 10)
head(mall.fuzzy$membership, 10)

mall.fuzzy$coeff # Dunn's partition coefficient

head(mall.fuzzy$clustering, 20)

# visualize
fviz_cluster(list(data=mall_sc, cluster=mall.fuzzy$clustering),
  palette = "",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.2, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6
  )

# model based clustering
require(ggpubr)
ggscatter(mallee, x='Clay', y='raw60', fill="red", pallette="Set1", mean.point=TRUE, color="Field.CO3") + geom_density2d(show.legend=TRUE)

require(mclust)
mall.modc <- Mclust(mall_sc)
summary(mall.modc)
