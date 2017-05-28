head(soils.sc)
str(soils.sc)
soils.sc <- as.data.frame(soils.sc)

require(factoextra)
require(NbClust)

# fviz_nbclust() function: Elbow, Silhouhette and Gap statistic methods
# Elbow method
fviz_nbclust(soils.sc, hcut, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2) +
  labs(subtitle = "Soils - Elbow method")

# Silhouette method
fviz_nbclust(soils.sc, hcut, method = "silhouette") +
  labs(subtitle = "Soils - Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
#set.seed(123)
fviz_nbclust(soils.sc, hcut, nstart = 10, method = "gap_stat", nboot = 150) +
  labs(subtitle = "Soils - Gap statistic method")

  # NbClust() function: 30 indices for choosing the best number of clusters
require("NbClust")
nb <- NbClust(soils.sc, distance = "maximum", min.nc = 2, max.nc = 10, method = "ward.D2")

# The result of NbClust using the function fviz_nbclust() [in factoextra], as follow:
fviz_nbclust(nb)
