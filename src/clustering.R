require(reshape2)
require(corrplot)
require(cluster)
require(ape)
require(DAAG)

col3 <- colorRampPalette(c("red","green","blue"))
corrplot(cor(mallee[, c(2:8,12:15,19:21)], use="p"), col=col3(20), type="lower", method="circle") 

corrplot(cor(mallee[, c(3,5,6,12:15,19:21)], use="p"), col=col3(20), type="lower", method="circle") 

mallee.1 <- mallee[, c(3,5,6,10,11:15,19:21)]
names(mallee.1)
names(mallee.1)[c(4,5)]  <- c('fldtxt','fec')
head(mallee.1)
corrplot(cor(mallee.1[,c(1:3,6:12)], use="p"), col=col3(20), type="lower", method="circle") 

mallee.1$textsc  <- as.numeric(mallee.1$fldtxt)
mallee.1$fecsc <- as.numeric(mallee.1$fec)
str(mallee.1)

corrplot(cor(mallee.1[,c(3,6:14)], use="p"), col=col3(20), type="lower", method="circle") 

raw2.dis <- daisy(mallee[,c(3,5,6,10,11,20)], metric="gower")
par(mar=c(3,4,5,3))
par(mfrow=c(1,3))
plot(hclust(raw2.dis, method="complete"), labels=mallee$Field.CO3, frame.plot=T, xlab="Groups", ylab="Distance", cex=0.6, main="Complete Linkage Clustering")
plot(hclust(raw2.dis, method="ward.D"), labels=mallee$Field.CO3, frame.plot=T, ylab="Distance", cex=0.6, main="Ward's' Clustering")
plot(hclust(raw2.dis, method="ward.D2"), labels=mallee$Field.CO3, frame.plot=T, ylab="Distance", cex=0.6, main="Ward's v2 Clustering")
par(mfrow=c(1,1))

initial <- hclust(raw2.dis, method="ward.D2")
plot(initial, labels=mallee$Field.CO3, frame.plot=T, xlab="Groups", ylab="Distance", cex=0.6, main="Ward's Clustering")

# draw dendogram with red borders around the 6, 8 & 10 clusters
rect.hclust(initial, k=6, border="red")
rect.hclust(initial, k=7, border="blue")
rect.hclust(initial, k=8, border="violet")

# compare hclust, agnes and diana method=complete
hclustc <- hclust(raw2.dis, method="ward.D2")
agnesc <- agnes(raw2.dis, method="complete")
dianac <- diana(raw2.dis)

par(mfrow=c(1,3))
plot(hclustc, labels=mallee$No, frame.plot=T, xlab="Hclust", ylab="Distance", cex=0.6, main="Heirarchical Clustering")
plot(agnesc,labels=mallee$No, which.plots=2, main="Agglomerative heirarchical", cex=0.6) 
plot(dianac,labels=mallee$No, which.plots=2, main="Divisive heirarchical", cex=0.6) 
par(mfrow=c(1,1))

# draw rectangles around 6 : 8 clusters
# agglomerative
plot(hclustc, labels=mallee$Field.CO3, frame.plot=T, xlab="Hclust", ylab="Distance", cex=0.6, main="Heirarchical Clustering")
rect.hclust(hclustc, k=6, border="red")
rect.hclust(hclustc, k=7, border="blue")
rect.hclust(hclustc, k=8, border="purple")

plot(agnesc,labels=mallee$Field.CO3, which.plots=2, main="Agglomerative heirarchical", cex=0.6) 
rect.hclust(agnesc, k=6, border="red")
rect.hclust(agnesc, k=7, border="blue")
rect.hclust(agnesc, k=8, border="purple")

plot(dianac,labels=mallee$Field.CO3, which.plots=2, main="Divisive heirarchical", cex=0.6) 
rect.hclust(dianac, k=6, border="red")
rect.hclust(dianac, k=7, border="blue")
rect.hclust(dianac, k=8, border="purple")

# plot as a phylogenic tree
pp1 <- as.phylo(as.hclust(hclustc), use.labels=T, labels=mallee$Field.CO3)
plot(pp1, cex=0.5, label.offset=0.001, font=1, direct='down', srt=180, adj=1.0, y.lim=c(-0.25, 2.05), main=" Heirarchical Dendrogram of Mallee Readily Available Water", cex.main=2.5)
groups8 <- cutree(hclustc, k=8)
tiplabels(col=groups8, pch=15, cex=1.25)
legend(x=1, y=2.075, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6','Group 7','Group 8'), bty='n', bg='gray', col=1:8, pch=15, title="Ward Clustering Soil Groups")
mallee$hclst <- groups8

pp2 <- as.phylo(as.hclust(agnesc))
plot(pp2, cex=0.5, label.offset=0.005, font=1, direct='down', srt=180, adj=1.0, y.lim=c(-0.20, 0.35), main="Agglomerative Dendrogram of Mallee Readily Available Water", cex.main=2.5)
groups8 <- cutree(agnesc, k=8)
tiplabels(col=groups8, pch=15, cex=1.25)
legend(x=1, y=2.075, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6','Group 7','Group 8'), bty='n', bg='gray', col=1:8, pch=15, title="Ward (agnes) clustering Soil Groups")
mallee$agnes <- groups8

pp3 <- as.phylo(as.hclust(dianac))
plot(pp3, cex=0.5, label.offset=0.005, font=1, direct='down', srt=180, adj=1.0, y.lim=c(-0.20, 0.3), main="Divisive Heirarchical Dendrogram of Mallee Readily Available Water", cex.main=2.5)
groups8 <- cutree(dianac, k=8)
tiplabels(col=groups8, pch=15, cex=1.25)
legend(x=1, y=0.275, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6','Group 7','Group 8'), bty='n', bg='gray', col=1:8, pch=15, title="Divisive Heirarchical Soil Groups")
mallee$diana <- groups8

head(mallee[order(mallee$hclst), c(3,5,6,10,11,20, 22:24)], 20)
mallee[order(mallee$hclst), c(3,5,6,10,11,20, 22:24)]
with(mallee, hclst==agnes)
hclust.tab <- with(mallee, table(Field.CO3, as.factor(hclst)))
agnes.tab <- with(mallee, table(Field.CO3, as.factor(agnes)))
diana.tab <- with(mallee, table(Field.CO3, as.factor(diana)))

# barplots
# Hcluster groups
par(mfrow=c(1,3))
barplot(hclust.tab, beside=T, legend=c('Nil','Slight','Medium','High','Very High'), col=browns, xlab="Soil Groups", ylab="Frequency")
title(main='Fine Earth Carbonate Class in each Hclust Soil Group')

# Agnes groups
barplot(agnes.tab, beside=T, legend=c('Nil','Slight','Medium','High','Very High'), col=browns, xlab="Soil Groups", ylab="Frequency")
title(main='Fine Earth Carbonate Class in each Agnes Soil Group')

# diana Groups
barplot(diana.tab, beside=T, legend=c('Nil','Slight','Medium','High','Very High'), col=browns, xlab="Soil Groups", ylab="Frequency")
title(main='Fine Earth Carbonate Class in each Diana Soil Group')
par(mfrow=c(1,1))

# aov
options(contrasts=c("contr.treatment","contr.helmert"))
hclst.lm01 <- lm(raw2~Field.CO3 + as.factor(hclst), data=mallee)
anova(hclst.lm01)
summary(hclst.lm01)
par(mfrow=c(2,2))
plot(hclst.lm01)
par(mfrow=c(1,1))

agnes.lm01 <- lm(raw2~Field.CO3 + as.factor(agnes), data=mallee)
anova(agnes.lm01)
summary(agnes.lm01)
par(mfrow=c(2,2))
plot(agnes.lm01)
par(mfrow=c(1,1))

diana.lm01 <- lm(raw2~Field.CO3 + as.factor(diana), data=mallee)
anova(diana.lm01)
summary(diana.lm01)
par(mfrow=c(2,2))
plot(diana.lm01)
par(mfrow=c(1,1))

anova(hclst.lm01, agnes.lm01)
anova(hclst.lm01, diana.lm01)
anova(agnes.lm01, diana.lm01)

with(mallee, tapply(raw2, list(Field.CO3, diana), mean, na.rm=T))
with(mallee, tapply(raw2, list(Field.CO3, Field.Texture), mean, na.rm=T))
subset(mallee[order(mallee$Field.Texture, mallee$Field.CO3),c(3,5,6,10,11,20,24)], diana==8)

# repeat diana clustering for total available water (taw)
taw.dis <- daisy(mallee[,c(3,5,6,10,11,21)], metric="gower")
dianat <- diana(taw.dis)
plot(dianat,labels=mallee$No, which.plots=2, main="Divisive heirarchical", cex=0.6) 

# plot as a phylogenic tree
pp4 <- as.phylo(as.hclust(dianat))
plot(pp4, cex=0.5, label.offset=0.005, font=1, direct='down', srt=180, adj=1.0, y.lim=c(-0.20, 0.3), main="Divisive Heirarchical Dendrogram of Mallee Total Available Water", cex.main=2.5)
groups8 <- cutree(dianat, k=8)
tiplabels(col=groups8, pch=15, cex=1.25)
legend(x=1, y=0.275, legend=c('Group 1','Group 2','Group 3','Group 4','Group 5','Group 6','Group 7','Group 8'), bty='n', bg='gray', col=1:8, pch=15, title="Divisive Heirarchical Soil Groups")
mallee$dianat <- groups8

# barplots
# diana Groups
dianat.tab <- with(mallee, table(Field.CO3, as.factor(dianat)))
barplot(dianat.tab, beside=T, legend=c('Nil','Slight','Medium','High','Very High'), col=browns, xlab="Soil Groups", ylab="Frequency")
title(main='Fine Earth Carbonate Class in each Diana Soil Group')

diana.lm02 <- lm(raw2~Field.CO3 + as.factor(dianat), data=mallee)
anova(diana.lm02)
summary(diana.lm02)
par(mfrow=c(2,2))
plot(diana.lm02)
par(mfrow=c(1,1))
