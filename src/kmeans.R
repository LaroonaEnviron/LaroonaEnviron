str(soils)

# normalize numeric variables
means <- apply(soils[, 1:11], 2, mean)
sds <- apply(soils[, 1:11], 2, sd)
soils.dat <- sweep(soils[, 1:11], 2, means, FUN="-")
soils.dat <- sweep(soils.dat, 2, sds, FUN="/")
head(round(soils.dat, 2), 10)

# find ideal no. of clusters
n <- length(soils.dat[, 1])
wss1 <- (n-1) * sum(apply(soils.dat[, 1:11], 2, var))
wss <- numeric(0)
for(i in 2:10) {
	   W <- sum(kmeans(soils.dat[, 1:11], i)$withinss)
	   wss <- c(wss, W)
}
#
wss <- c(wss1, wss)
plot(1:10, wss, type="l", xlab="Number of groups", ylab="Within groups sum of squares", lwd=2)

# add cluster group to data
soils.kmean <- kmeans(soils.dat, 6)
print(soils.kmean)

dnames <- dimnames(soils.dat)[[2]]
soils.dat <- cbind(soils.dat, soils.kmean$cluster)

dimnames(soils.dat)[[2]] <- c(dnames, "cluster")
head(soils.dat, 20)

# get mean of clusters for raw data
lapply(1:6,function(nc) apply(soils[soils.kmean$cluster==nc, 1:11],2,mean))

soils$cluster <- soils.dat$cluster
str(soils) 

with(soils, table(Field.CO3, as.factor(cluster)))
with(soils, table(Region, as.factor(cluster)))
xtabs( ~ Field.CO3 + Field.Texture + as.factor(cluster), data=soils)

ggplot(data=soils, aes(upper, KPa8, colour=as.factor(cluster))) + geom_point(size=2)

soils$hcpc <- as.integer(soils.hcpc$data.clust$clust)
plot(soils$cluster, soils$hcpc)