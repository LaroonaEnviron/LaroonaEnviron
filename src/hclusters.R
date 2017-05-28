clay.ave <- with(Soils, tapply(Clay, Field.Texture, mean, na.rm=T))
sand.ave <- with(Soils, tapply(Sand, Field.Texture, mean, na.rm=T))
clay.ave
sand.ave
ratio <- sand.ave / clay.ave
ratio/ratio[1]

mallee$raw1 <- with(mallee, (KPa8-KPa40)*1000)
mallee$raw2 <- with(mallee, (KPa8-KPa60)*1000)
mallee$taw  <- with(mallee, (KPa8-KPa1500)*1000)
str(mallee)

# calculate distance matrix
head(mallee[, c(2:8,12:15,19:21)])
round(cor(mallee[, c(2:8,12:15,19:21)]), 2)
require(corrgram)
corrgram(mallee[, c(2:8,12:15,19:21)], order=T, upper.panel=panel.pts, pch=16)
raw2.d <- dist(mallee[, c(2:8,12:15,19:21)])

par(mfrow=c(1,3))
plot(hclust(raw2.d, method="complete"),labels=mallee$No, frame.plot=T, xlab="Groups", ylab="Distance", cex=0.5, main="Complete Linkage")
plot(hclust(raw2.d, method="ward.D"),labels=mallee$No, frame.plot=T, ylab="Distance", cex=0.5, main="Single Linkage")
plot(hclust(raw2.d, method="ward.D2"),labels=mallee$No, frame.plot=T, ylab="Distance", cex=0.5, main="Ward.D2")
par(mfrow=c(1,1))

# use complete linkage
initial <- hclust(d, method="complete")
plot(initial,labels=mallee$No, ylab="Distance", cex=0.5)
grps_six_1 <- cutree(initial, k=6)
grps_six_2 <- cutree(initial, h=100)
which(grps_six_1==grps_six_2)

soil.names <- row.names(mallee)
raw.grps_1 <- lapply(1:6, function(nc) soil.names[grps_six_1==nc])
raw.grps_2 <- lapply(1:6, function(nc) soil.names[grps_six_2==nc])

# draw dendogram with red borders around the 5, 6 & 10 clusters
rect.hclust(initial, k=5, border="red")
rect.hclust(initial, k=6, border="blue")
rect.hclust(initial, k=10, border="green")

require(cluster)
str(mallee)
head(mallee[, c(2,3,5:8,10:12,14,20)], 20)
raw2.dis <- daisy(mallee[, c(2,3,5:8,10:12,14,20)], metric="gower")
summary(raw2.dis)

par(mfrow=c(1,3))
plot(hclust(raw2.dis, method="complete"),labels=mallee$Field.Texture, frame.plot=T, xlab="Groups", ylab="Distance", cex=0.5, main="Complete Linkage")
plot(hclust(raw2.dis, method="median"),labels=mallee$No, frame.plot=T, ylab="Distance", cex=0.5, main="Median")
plot(hclust(raw2.dis, method="average"),labels=mallee$No, frame.plot=T, ylab="Distance", cex=0.5, main="Average")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
plot(agnes(raw2.dis, method="complete"),labels=mallee$No, which.plots=2, main="Complete", cex=0.6) 
plot(agnes(raw2.dis, method="average"),labels=mallee$No, which.plots=2, main="Average", cex=0.6) 
plot(diana(raw2.dis),labels=mallee$No, which.plots=2, main="Divisive heirarchical", cex=0.6) 
par(mfrow=c(1,1))

raw2.div <- diana(raw2.dis)
plot(raw2.div, which.plot=2, cex=0.6, labels=mallee$No, main="Divisive heirarchical")
rect.hclust(raw2.div, k=6)
rect.hclust(raw2.div, k=10, border="blue")

require(sp)
require(ape)
require(cluster)
require(DAAG)

pp <- as.phylo(as.hclust(raw2.div))
plot(pp, cex=0.5, label.offset=0.025, font=1, direct='down', srt=180, adj=1.10, y.lim=c(-0.25, 0.5), main="Divisive Heirarchical Dendrogram of Mallee Readily Available Water", cex.main=1.5)
groups6 <- cutree(raw2.div, k=6)
tiplabels(col=groups6, pch=15, cex=1.25)
legend(x=1, y=0.5, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6'), bty='n', bg='gray', col=1:6, pch=15, title="Numerical Soil Groups")
mallee$grp6.d <- groups6
str(mallee)
head(mallee[order(mallee$raw2),c(3,5:7, 10, 11, 20, 23)], 50)
with(mallee, mallee[order(Field.Texture, Field.CO3),c(3,5:7, 10, 11, 20, 23)])

for (i in 1:6) {
  print(mallee[mallee$grp6.d==i, c(3,5:7, 10, 11, 20, 22,23)])
  pause()
  }

with(mallee, {
  print(round(tapply(raw2, grp6.d, mean),0))
  print(round(tapply(raw2, grp6.d, sd),0))
  print(round(tapply(raw2, grp6.d, length),0))
  })

# repeat for agglomerative clustering
raw.agg <- agnes(raw2.dis)

pp <- as.phylo(as.hclust(raw.agg))
plot(pp, cex=0.5, label.offset=0.005, font=1, direct='down', srt=180, adj=1.10, y.lim=c(-0.15,0.05), main="Agglomerative Heirarchical Dendrogram of Mallee Readily Available Water", cex.main=1.75)
groups6 <- cutree(raw.agg, k=6)
tiplabels(col=groups6, pch=15, cex=1.25)
legend(x=15, y=3.0, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6'), bty='n', bg='gray', col=1:6, pch=22, title="Numerical Soil Groups")
mallee$grp60.a <- groups6

for (i in 1:10) {                                         
  print(mallee[mallee$grp60.a==i, c(3,5:7, 10, 11, 20, 21:24)])
  pause()
  }

with(mallee, {
  print(round(tapply(raw2, grp60.a, mean),0))
  print(round(tapply(raw2, grp60.a, sd),0))
  print(round(tapply(raw2, grp60.a, length),0))
  })


# carry out a manova
raw2.lm01 <- lm(raw2~factor(grp10.d), data=mallee)
anova(raw2.lm01)
summary(raw2.lm01)

raw2.lm02 <- lm(raw2~factor(grp6.d), data=mallee)
anova(raw2.lm02, raw2.lm01)
anova(raw2.lm02)
summary(raw2.lm02)

raw2.lm03 <- lm(raw2~factor(grp60.a), data=mallee)
anova(raw2.lm03, raw2.lm02)
anova(raw2.lm03)
summary(raw2.lm03)

# use full Soils dataset
str(Soils)
Soils$raw1 <- with(Soils, (KPa8-KPa40)*1000)
Soils$raw2 <- with(Soils, (KPa8-KPa60)*1000)
Soils$taw  <- with(Soils, (KPa8-KPa1500)*1000)
str(Soils)

# construct dissimilarity matrix
tail(Soils[, c(1,3,5:8,20)], 30)
raw2.dis <- daisy(Soils[!Soils$No==335, c(3,5:8,20)], stand=TRUE)

par(mfrow=c(1,3))
plot(agnes(raw2.dis, method="complete"),labels=Soils$No, which.plots=2, main="Complete", cex=0.6) 
plot(agnes(raw2.dis, method="average"),labels=Soils$No, which.plots=2, main="Average", cex=0.6) 
plot(diana(raw2.dis),labels=Soils$No, which.plots=2, main="Divisive heirarchical", cex=0.6) 
par(mfrow=c(1,1))

raw.div <- diana(raw2.dis)
plot(raw.div, which.plot=2, cex=0.6, labels=Soils$No[!Soils$No==335], main="Divisive heirarchical")
rect.hclust(raw.div, k=5)
rect.hclust(raw.div, k=10, border="blue")

pp <- as.phylo(as.hclust(raw.div))
plot(pp, cex=0.5, label.offset=0.01, font=1, direct='down', srt=180, adj=1.0, y.lim=c(-0.1, 7.0), main="Divisive Heirarchical Dendrogram of Mallee Readily Avaialble Water", cex.main=1.5)
groups10 <- cutree(raw.div, k=10)
tiplabels(col=groups10, pch=15, cex=1.25)
legend(x=5, y=6.5, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6','Group 7','Group 8','Group 9','Group 10'), bty='n', bg='gray', col=1:10, pch=15, title="Numerical Soil Groups")
Soils$grp10[!Soils$No==335] <- groups10
str(Soils)
head(Soils[,c(3,5:7, 10, 11, 20, 22)], 50)
tail(Soils[,c(3,5:7, 10, 11, 20, 22)], 50)

with(Soils, round(tapply(raw2, grp10, mean),0))
with(Soils, tapply(raw2, grp10, length))
