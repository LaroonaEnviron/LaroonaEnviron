if(!require(fpc)) install.packages("fpc")
if(!require(dbscan))  install.packages("dbscan")
require(factoextra)

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(soils.sc, eps = 1.05, MinPts = 5)
print(db)

# Plot DBSCAN results
fviz_cluster(db, data = soils.sc, stand = FALSE,
  ellipse = TRUE, show.clust.cent = TRUE,
  geom = "point", palette = "jco", ggtheme = theme_bw())

dbscan::kNNdistplot(soils.sc, k = 5)
abline(h = 1, lty = 2)  