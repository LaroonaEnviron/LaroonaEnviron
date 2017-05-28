require("factoextra")
require("FactoMineR")
require(NbClust)

# Normalization function
normalize <- function(x=x, fun="z-score") {
    if(fun=="z-score") 
        return (scale(x))  # normalisation mean=0, sd=1
    if (fun== "min-max") 
        return ((x - min(x)) / (max(x) - min(x))) # max - min standardisation
    if(fun=="median") 
        return ((x-median(x))/IQR(x)) # median standardisation
  }
  
# max - min standardisation
mall.sc1 <- as.data.frame(lapply(mallee[,1:13], normalize, "min-max"))

# Z-Score Standardization
mall.sc2 <- as.data.frame(lapply(mallee[,1:13], normalize, "z-score"))

# median standardization
mall.sc3 <- as.data.frame(lapply(mallee[,1:13], normalize, "median" ))

# Elbow method - maxmin
fviz_nbclust(mall.sc1, kmeans, method = "wss") +
                 geom_vline(xintercept = 6, linetype = 2) +
                 labs(subtitle = "Mallee Soil - Elbow method - maxim")

# Elbow method - z-score
fviz_nbclust(mall.sc2, kmeans, method = "wss") +
                 geom_vline(xintercept = 6, linetype = 2) +
                 labs(subtitle = "Mallee Soil - Elbow method - z-score")

# Elbow method - maxmin
fviz_nbclust(mall.sc3, kmeans, method = "wss") +
                 geom_vline(xintercept = 6, linetype = 2) +
                 labs(subtitle = "Mallee Soil - Elbow method - median")

fviz_pca_ind(PCA(mall.sc1), title = "PCA - Mallee soils data- maxmin",
    habillage=mallee$Field.CO3, palette = "uscgb",
    geom = "point", ggtheme = theme_classic(),
    legend = "top")

fviz_pca_ind(PCA(mall.sc2), title = "PCA - Mallee soils data- z-score",
    habillage=mallee$Field.CO3, palette = "uscgb",
    geom = "point", ggtheme = theme_classic(),
    legend = "top")

fviz_pca_ind(PCA(mall.sc3), title = "PCA - Mallee soils data- median",
    habillage=mallee$Field.CO3, palette = "uscgb",
    geom = "point", ggtheme = theme_classic(),
    legend = "top")
