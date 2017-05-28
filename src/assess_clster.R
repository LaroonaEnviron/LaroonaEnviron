head(soils[, 1:11])
head(soils.sc)
str(soils)

require("factoextra")

# Plot faithful data set
fviz_pca_ind(prcomp(soils.sc), title = "PCA - Soil data",
    habillage = soils$cluster, palette = "RdBu",
    geom = "point", ggtheme = theme_classic(),
    legend = "bottom")

# K-means on soils dataset
soils.km1 <- kmeans(soils.sc, 6)
fviz_cluster(list(data = soils.sc, cluster = soils.km1$cluster),
    ellipse.type = "norm", geom = "point", stand = FALSE,
    palette = "jco", ggtheme = theme_bw())

# Hierarchical clustering on the iris dataset
fviz_dend(hclust(dist(soils.sc)), k = 7, k_colors = "jco",
    as.ggplot = TRUE, show_labels = TRUE)

require(clustertend)

# Compute Hopkins statistic for iris dataset
set.seed(123)
hopkins(soils.sc, n = nrow(soils.sc)-1)

# Visual methods
fviz_dist(dist(soils.sc), show_labels = TRUE, gradient=list(low="red", mid="grey", high="navyblue")) + labs(title = "Soils data")

