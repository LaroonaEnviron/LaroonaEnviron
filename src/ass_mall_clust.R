require("factoextra")
require(NbClust)

# assessing mallee soils to cluster
# scale the mallee data set

str(mallee)
mall_sc <- scale(mallee[, -c(3, 6, 9, 10, 14:16)])
mall_sc <- data.frame(mall_sc)
str(mall_sc)
summary(mall_sc)

# Elbow method
fviz_nbclust(mall_sc, kmeans, method = "wss") +
                 geom_vline(xintercept = 7, linetype = 2) +
                 labs(subtitle = "Mallee Soil - Elbow method")

# Silhouette method
fviz_nbclust(mall_sc, kmeans, method = "silhouette")+
                 labs(subtitle = "Mallee Soil - Silhouette method")
                 
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(111)
fviz_nbclust(mall_sc, kmeans, nstart = 25, method = "gap_stat", nboot = 250) +
                 labs(subtitle = "Mallee Soil - Gap statistic method")

# determine optimal number of clusters
set.seed(111)
require(FactoMineR)

# plot the scale mallee data sets
fviz_pca_ind(PCA(mall_sc), title = "PCA - Mallee soils data",
    habillage=mallee$Field.CO3, palette = "uscgb",
    geom = "point", ggtheme = theme_classic(),
    legend = "top")

fviz_pca_biplot(prcomp(mall_sc), title = "PCA - Mallee soils data",
    habillage=mallee$Field.CO3, palette = "uscgb",
    geom = "point", ggtheme = theme_classic(),
    legend = "top")

require(clustertend)

# Compute Hopkins statistic for iris dataset
set.seed(123)
hopkins(mall_sc, n = nrow(mall_sc)-1)

# Visual methods
fviz_dist(dist(mall_sc), show_labels = FALSE, gradient=list(low="red", mid="white", high="blue")) + labs(title = "Mallee soils data")

nb <- NbClust(mall_sc, distance = "euclidean", min.nc = 2,
max.nc = 10, method = "kmeans")
