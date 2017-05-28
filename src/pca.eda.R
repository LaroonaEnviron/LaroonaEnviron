require(FactoMineR)
require(missMDA)

missSoils <- imputePCA(soils[, 1:13], ncp=8)
missSoils$completeObs[missBD]

missBD <- which(is.na(soils$BD))

soils$BD[missBD] <- round(missSoils$completeObs[missBD], 2)
rm(missBD, missSoils, nb)
summary(soils)

# do a principal components analysis on the soils data
soils.pca <- PCA(soils, scale.unit=TRUE, quali.sup=c(14:16))
summary(soils.pca)

plot(soils.pca, choix="ind", habillage=14, cex=0.7, invisible="ind")
plot(soils.pca, choix="ind", habillage=15, cex=0.7, invisible="ind", new.plot=TRUE)
plot(soils.pca, choix="var", habillage=14, cex=0.6, invisible="ind")

str(soils[ , -c(3, 6, 9, 10)])
soils.pca01 <- PCA(soils, scale.unit=TRUE, quanti.sup=c(3, 6, 9, 10), quali.sup=c(14:16))
summary(soils.pca01)

plot(soils.pca01, choix="ind", habillage=10, cex=0.6, xlim=c(-2, 5), invisible="var")
plot(soils.pca01, choix="ind", habillage=11, cex=0.6)
plot(soils.pca01, choix="var", habillage=12, cex=0.6)

plotellipses(soils.pca01, level=0.99)
