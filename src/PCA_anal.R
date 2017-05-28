require(FactoMineR)
require(missMDA)

(nb <- estim_ncpPCA(soils[, 1:11], ncp.max=4))
missSoils <- imputePCA(soils[, 1:11], ncp=4)
missSoils$completeObs[missBD]

missBD <- which(is.na(soils$BD))

str(soils)
str(soils[, c(-3, -6, -9, -10, -13:-15)])
head(soils[, c(-3, -6, -9, -10, -13:-15)])
msoils <- soils
soils$BD[missBD] <- round(missSoils$completeObs[missBD], 2)
str(soils)

head(soils[, -c(12:15, 19)])
names(soils[, -c(12:15, 19)])

# do a principal components analysis on the soils data
soils.pca <- PCA(soils[, -c(12:15, 19)], scale.unit=TRUE, quanti.sup=c(3,6,9,10), quali.sup=c(12:14))
summary(soils.pca)

plot(soils.pca, choix="ind", habillage=12, cex=0.7, invisible="ind")
plot(soils.pca, choix="ind", habillage=13, cex=0.7, invisible="var", new.plot=TRUE)

soils.pca <- PCA(soils[, -c(12:15, 19)], scale.unit=TRUE, quanti.sup=c(3,6,9,10), quali.sup=12:14, ncp=3)

# do heirarchical tree
soils.hcpc50 <- HCPC(soils.pca, kk=50, consol=F) 
summary(soils.hcpc)

str(soils.hcpc$data.clust)
table(soils.hcpc50$data.clust$clust, soils.hcpc$data.clust$clust)
soils.hcpc$desc.var
