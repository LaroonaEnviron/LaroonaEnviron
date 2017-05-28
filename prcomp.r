csvfile <- choose.files("*.csv")
mrc <- read.csv(csvfile, header=T)
str(mrc)
head(mrc,10)
rnames <- as.character(mrc[,2])
mrc$Name <- NULL
rownames(mrc) <- rnames
attach(mrc)
mrc$Texture <- ordered(Texture, levels=c("S","LS","SL","L","SCL","CL","C"))
mrc$Field_Text <- ordered(Field_Text, levels=c("S","LS","SL","LSCL","SCL","CL","C"))
mrc$Field_CO3 <- ordered(Field_CO3, levels=c("N","S","M","H","V"))
detach(mrc)
mrc$raw1 <- with(mrc, Psi8-Psi40)
mrc$raw2 <- with(mrc, Psi8-Psi60)
mrc$taw <- with(mrc, Psi8-Psi1500)
pairs(mrc[,c(2:6,9)], pch=16, col=as.integer(mrc$Field_Text))
legend(locator(1), legend=c("S","LS","SL","LSCL","SCL","CL","C"), col=as.integer(mrc$Field_Text))
pairs(mrc[,c(12:18)], pch=16, col=as.integer(mrc$Field_Text))
summary(mrc)
par(mfrow=c(2,2))
hist(mrc$Clay^0.75, col="orange", density=15, angle=60)
hist(mrc$Sand^0.75, col="brown", density=15, angle=60)
hist(mrc$BD^0.5, col="red", density=15, angle=150)
hist(mrc$Lab_CO3^0.5, col="gray", density=15, angle=150)

hist(mrc$Psi8, col="orange", density=15, angle=60)
hist(mrc$Psi40, col="brown", density=15, angle=60)
hist(mrc$Psi60, col="red", density=15, angle=150)
hist(mrc$Psi1500, col="gray", density=15, angle=150)
par(mfrow=c(1,1))

cor(mrc[,c(2:6,9,12:18)])
cov(mrc[,c(2:6,9,12:18)])
require(MASS)
(mrc.pca <- princomp(sqrt(mrc[,c(4:6,9,12)]), cor=T))
summary(mrc.pca)
plot(mrc.pca)
loadings(mrc.pca)
mrc.pdt <- predict(mrc.pca)
eqscplot(mrc.pdt[,1:2], type="n", xlab="first principal component", ylab="second principal component")
text(mrc.pdt[,1:2], labels=as.character(mrc$Field_Text), col=as.integer(mrc$Field_Text)+1, cex=0.5)

plot(mrc$Psi60,mrc.pdt[,1], pch=16, col=as.integer(mrc$Field_Text)+1, cex=0.5)

library(MASS= first=T)
biplot(mrc.pca, pc.biplot=T,cex=0.5, expand=1.0)

with(mrc, plot(Sand, raw1, pch=16, col=as.integer(Field_Text)+1))
round(sd.psi <- sqrt(var(mrc$Sand)),3)
round(sd.raw <- sqrt(var(mrc$raw1)),3)

head(mrc,50)
mrc.srt <- mrc[order(mrc$Sand),]
head(mrc.srt,50)

 