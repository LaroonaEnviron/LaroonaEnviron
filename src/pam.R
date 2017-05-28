#mediod clustering

require(factoextra)
require(cluster)

# suggest optimal number of clusters
fviz_nbclust(soils.sc, pam, method = "silhouette", verbose=TRUE) + theme_classic()

# determine number of clusters = 6

soils.pam <- pam(soils.sc, 6)
print(soils.pam)

soils.pam$medoids

# Cluster numbers
head(soils.pam$clustering, 10)

# add points to original data
dd <- cbind(soils, cluster = soils.pam$cluster)
head(dd, n = 10)

# visualize the clusters
fviz_cluster(soils.pam,
palette = c("red", "orange", "brown", "green", "blue", "purple"), # color palette
ellipse.type = "t", # Concentration ellipse
repel = TRUE, # Avoid label overplotting (slow)
main="Mediod Clustering", # Main title
ggtheme = theme_bw()
)
