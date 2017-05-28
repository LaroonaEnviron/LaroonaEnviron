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

# claasify using rpart
mrc.srt <- with(mrc, mrc[order(Field_Text, Field_CO3),])
head(mrc.srt,20)
par(mfrow=c(1,3))
with(mrc.srt, { # field texture
  boxplot(raw1*1000~Field_Text, notch=F, col="lightblue", horizontal=F, main="Available Moisture, 8-40 kPa",
    xlab="Field Texture", ylab="Available Moisture (mm/m)")
  boxplot(raw2*1000~Field_Text, notch=F, col="grey", horizontal=F, main="Available Moisture, 8-60 kPa",
    xlab="Field Texture", ylab="Available Moisture (mm/m)")
  boxplot(taw*1000~Field_Text, notch=F, col="brown", horizontal=F, main="Available Moisture, 8-1500 kPa",
    xlab="Field Texture", ylab="Available Moisture (mm/m)")
  })
par(mfrow=c(1,1))

par(mfrow=c(1,3))
with(mrc.srt, { # field carbonate
  boxplot(raw1*1000~Field_CO3, notch=F, col="lightblue", horizontal=F, main="Available Moisture, 8-40 kPa",
    xlab="Field CO3", ylab="Available Moisture (mm/m)")
  boxplot(raw2*1000~Field_CO3, notch=F, col="grey", horizontal=F, main="Available Moisture, 8-60 kPa",
    xlab="Field CO3", ylab="Available Moisture (mm/m)")
  boxplot(taw*1000~Field_CO3, notch=F, col="brown", horizontal=F, main="Available Moisture, 8-1500 kPa",
    xlab="Field CO3", ylab="Available Moisture (mm/m)")
  })
par(mfrow=c(1,1))

# classification tree - classify on Field Texture and Field carbonate
# response variable = raw1 - available water between 8 - 40 kPa
require(MASS)
boxcox(sqrt(raw1*1000)~Field_Text+Field_CO3, data=mrc.srt, lambda=seq(-2,2,length=30))
require(rpart)
raw1.rp <- rpart(round(raw1*1000,0)~Field_Text+Field_CO3+Top, data=mrc, method="anova", model=T, cp=0.001)
print(raw1.rp)
plot(raw1.rp)
text(raw1.rp, cex=0.75, col=c(1:5,8:11))
printcp(raw1.rp)
plotcp(raw1.rp)

with(raw1.rp, {
  plot(cptable[,3], xlab="Tree Number", ylab="Resubstitution Error", type="b")
  par(new=T)
  plot(cptable[,4], xlab="Tree Number", ylab="Cross-validated Error", type="b", col="blue")
  })

plotcp(raw1.rp, lty=2,col="black")
with(raw1.rp, {
  lines(cptable[,2]+1, cptable[,3], type="b", col="red")
  legend(locator(1), c("Resub. Error","CV Error","min(CV Error)+1SE"),lty=c(1,2,2), col=c(2,4,4),bty=  "n")
  })

raw1.prune <- prune(raw1.rp, cp=0.02)
summary(raw1.prune)

plot(raw1.prune, uniform=T, branch=0.05, main="Version B")
text(raw1.prune, pretty=1, use.n=F, col=2:5, cex=0.75)

plot(raw1.prune, uniform=T, branch=0.5, margin=0.05, main="Version C")
text(raw1.prune, pretty=1, all=T, use.n=F, fancy=T, col=2:5, cex=0.75)
   