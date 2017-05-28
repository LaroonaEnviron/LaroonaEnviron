require(RColorBrewer)
require(car)
require(xlsx)

addrows <- function(dfSum) {
# reconfigure summary dataframe to add missing factors
  tmp <- dfSum
  dfSum[4,1:2] <- c('S','H')
  dfSum[4,3:7] <- rep(NA,5)
  dfSum[5,1:2] <- c('S','V')
  dfSum[5,3:7] <- rep(NA,5)
  dfSum[6:9,]  <- tmp[4:7,]
  dfSum[10,1:2] <- c('LS','V')
  dfSum[10,3:7] <- rep(NA,5)
  dfSum[11:(nrow(tmp)+3),]  <- tmp[8:nrow(tmp),]
  return (dfSum)
  }
  
# plot out available water holding capacities
# summarize available water categories
raw1Sum <- summarySE(mallee, "raw1", c("Field.Texture","Field.CO3"))
raw2Sum <- summarySE(mallee, "raw2", c("Field.Texture","Field.CO3"))
tawSum <- summarySE(mallee, "taw", c("Field.Texture","Field.CO3"))

# add to summary dataframe missing factors for later consistent plotting
raw1Sum <- addrows(raw1Sum)
raw2Sum <- addrows(raw2Sum)
tawSum <- addrows(tawSum)
  
# define color ranges
browns <- brewer.pal(9, "YlOrBr")
blues <- brewer.pal(9, "Blues")[c(4:8)]
greys <- brewer.pal(9, "Greys")[c(3:7)]

# plot bar graph with +/-se with ggplot
ggplot(raw1Sum, aes(x=Field.Texture, y=raw1, fill=Field.CO3)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=raw1-se, ymax=raw1+se),  width=.2, position=position_dodge(.9)) + scale_fill_manual(values=greys)

# barplot
barplot(raw1Sum$raw1, beside=TRUE, legend.text=TRUE, col=browns, xlab="Texture Grades", ylab="Available Water (mm/m)", border="black")
text(45,70, labels="Fine\n Earth\n Carbonate\n Classes" )
title(main="Effect of Fine Earth Carbonates on Available Water Content\n Mallee Soils")

#================ analyse for raw1 - 8 - 40 kPa ======================
# linear regression on each hand texture class
Sands <- subset(mallee, Field.Texture=="S")
Sands$Field.CO3 <- factor(Sands$Field.CO3, levels=c("N", "S", "M"), ordered=T)
summary(powerTransform(raw1~Field.CO3, data=Sands))
boxcox(raw1^(13.8/10)~Field.CO3, data=Sands)
contrasts(Sands$Field.CO3) <- contr.treatment(3)

sand.lm01 <- lm(raw1^(13.8/10)~Field.CO3, data=Sands)
anova(sand.lm01)
summary(sand.lm01)
par(mfrow=c(2,2))
plot(sand.lm01)
par(mfrow=c(1,1))
with(Sands, tapply(raw1, Field.CO3, mean, na.rm=T))

# Field texture is LS
LSand <- subset(mallee, Field.Texture=="LS")
LSand$Field.CO3 <- factor(LSand$Field.CO3, levels=c("N", "S", "M", "H"), ordered=T)
summary(powerTransform(raw1~Field.CO3, data=LSand))
boxcox(raw1^(3.6/10)~Field.CO3, data=LSand)
contrasts(LSand$Field.CO3) <- contr.treatment(4)

loamysand.lm01 <- lm(raw1^(3.6/10)~Field.CO3, data=LSand)
anova(loamysand.lm01)
summary(loamysand.lm01)
par(mfrow=c(2,2))
plot(loamysand.lm01)
par(mfrow=c(1,1))
with(LSand, tapply(raw1, Field.CO3, mean, na.rm=T))

# Field texture is SL
SandL <- subset(mallee, Field.Texture=="SL")
summary(powerTransform(raw1~Field.CO3, data=SandL))
boxcox(raw1^(0.58/10)~Field.CO3, data=SandL)
contrasts(SandL$Field.CO3) <- contr.treatment(5)

sandyloam.lm01 <- lm(raw1^(0.58/10)~Field.CO3, data=SandL)
anova(sandyloam.lm01)
summary(sandyloam.lm01)
par(mfrow=c(2,2))
plot(sandyloam.lm01)
par(mfrow=c(1,1))
with(SandL, tapply(raw1, Field.CO3, mean, na.rm=T))

# Field texture is LSCL
LSCLoam <- subset(mallee, Field.Texture=="LSCL")
contrasts(LSCLoam$Field.CO3) <- contr.treatment(5)
summary(powerTransform(raw1~Field.CO3, data=LSCLoam))
boxcox(raw1^(1/3)~Field.CO3, data=LSCLoam)

lscl.lm01 <- lm(raw1^(1/3)~Field.CO3, data=LSCLoam)
anova(lscl.lm01)
summary(lscl.lm01)
par(mfrow=c(2,2))
plot(lscl.lm01)
par(mfrow=c(1,1))
with(LSCLoam, tapply(raw1, Field.CO3, mean, na.rm=T))

# Field texture is SCL
SCLoam <- subset(mallee, Field.Texture=="SCL")
summary(powerTransform(raw1~Field.CO3, data=SCLoam))
boxcox(raw1^(-7.8/10)~Field.CO3, data=SCLoam)
contrasts(SCLoam$Field.CO3) <- contr.treatment(5)

scloam.lm01 <- lm(raw1^(-7.8/10)~Field.CO3, data=SCLoam)
anova(scloam.lm01)
summary(scloam.lm01)
par(mfrow=c(2,2))
plot(scloam.lm01)
par(mfrow=c(1,1))
with(SCLoam, tapply(raw1, Field.CO3, mean, na.rm=T))

# Field texture is CL
ClayLoam <- subset(mallee, Field.Texture=="CL")
summary(powerTransform(raw1~Field.CO3, data=ClayLoam))
boxcox(raw1^(7/10)~Field.CO3, data=ClayLoam)
contrasts(ClayLoam$Field.CO3) <- contr.treatment(5)

clayloam.lm01 <- lm(raw1^(7/10)~Field.CO3, data=ClayLoam)
anova(clayloam.lm01)
summary(clayloam.lm01)
par(mfrow=c(2,2))
plot(clayloam.lm01)
par(mfrow=c(1,1))
with(ClayLoam, tapply(raw1, Field.CO3, mean, na.rm=T))

# Field texture is C
Clay <- subset(mallee, Field.Texture=="C")
summary(powerTransform(raw1~Field.CO3, data=Clay))
boxcox(raw1~Field.CO3, data=Clay)
contrasts(Clay$Field.CO3) <- contr.treatment(5)

clay.lm01 <- lm(raw1~Field.CO3, data=Clay)
anova(clay.lm01)
summary(clay.lm01)
par(mfrow=c(2,2))
plot(clay.lm01)
par(mfrow=c(1,1))
with(Clay, tapply(raw1, Field.CO3, mean, na.rm=T))

# All Field textures 
summary(powerTransform(raw1~Field.CO3*Field.Texture, data=mallee))
boxcox(raw1^(4/10)~Field.CO3*Field.Texture, data=mallee)
contrasts(mallee$Field.CO3) <- contr.treatment(5)
contrasts(mallee$Field.Texture) <- contr.treatment(7)

alltext.lm01 <- lm(raw1^(4/10)~Field.CO3*Field.Texture, data=mallee)
anova(alltext.lm01)
summary(alltext.lm01)
par(mfrow=c(2,2))
plot(alltext.lm01)
par(mfrow=c(1,1))
ave_raw1 <- with(mallee, tapply(raw1, list(Field.Texture,Field.CO3), mean, na.rm=T))

# write to Excel file
write.xlsx(ave_raw1, file="D:/Laroona Consulting/MRC/data/Average RAWs.xlsx", sheetName="raw1")

#================ analyse for raw2 - 8 - 60 kPa ======================
ggplot(raw2Sum, aes(x=Field.Texture, y=raw2, fill=Field.CO3)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=raw2-se, ymax=raw2+se),  width=.2, position=position_dodge(.9)) + scale_fill_manual(values=browns)

# linear regression on each hand texture class
summary(powerTransform(raw2~Field.CO3, data=Sands))
boxcox(raw2^(13.4/10)~Field.CO3, data=Sands)
contrasts(Sands$Field.CO3) <- contr.treatment(3)
sand.lm02 <- lm(raw2^(13.4/10)~Field.CO3, data=Sands)
anova(sand.lm02)
summary(sand.lm02)
par(mfrow=c(2,2))
plot(sand.lm02)
par(mfrow=c(1,1))
with(Sands, tapply(raw2^(13/10), Field.CO3, mean, na.rm=T))

# Field texture is LS
summary(powerTransform(raw2~Field.CO3, data=LSand))
boxcox(raw2^(3.6/10)~Field.CO3, data=LSand)
contrasts(LSand$Field.CO3) <- contr.treatment(4)
loamysand.lm02 <- lm(raw2^(3.6/10)~Field.CO3, data=LSand)
anova(loamysand.lm02)
summary(loamysand.lm02)
par(mfrow=c(2,2))
plot(loamysand.lm02)
par(mfrow=c(1,1))
with(LSand, tapply(raw2^(4/10), Field.CO3, mean, na.rm=T))

# Field texture is SL
summary(powerTransform(raw2~Field.CO3, data=SandL))
boxcox(raw2^(-2.2/10)~Field.CO3, data=SandL)
contrasts(SandL$Field.CO3) <- contr.treatment(5)
sandyloam.lm02 <- lm(raw2^(-2.2/10)~Field.CO3, data=SandL)
anova(sandyloam.lm02)
summary(sandyloam.lm02)
par(mfrow=c(2,2))
plot(sandyloam.lm02)
par(mfrow=c(1,1))
with(SandL, tapply(raw2^(-2.2/10), Field.CO3, mean, na.rm=T))

# Field texture is LSCL
contrasts(LSCLoam$Field.CO3) <- contr.treatment(5)
summary(powerTransform(raw2~Field.CO3, data=LSCLoam))
boxcox(raw2^(4.5/10)~Field.CO3, data=LSCLoam)
lscl.lm02 <- lm(raw2^(4.5/10)~Field.CO3, data=LSCLoam)
anova(lscl.lm02)
summary(lscl.lm02)
par(mfrow=c(2,2))
plot(lscl.lm02)
par(mfrow=c(1,1))

with(LSCLoam, tapply(raw2^(4.5/10), Field.CO3, mean, na.rm=T))

# Field texture is SCL
summary(powerTransform(raw2~Field.CO3, data=SCLoam))
boxcox(raw2^(-3.7/10)~Field.CO3, data=SCLoam)
contrasts(SCLoam$Field.CO3) <- contr.treatment(5)
scloam.lm02 <- lm(raw2^(-3.7/10)~Field.CO3, data=SCLoam)
anova(scloam.lm02)
summary(scloam.lm02)
par(mfrow=c(2,2))
plot(scloam.lm02)
par(mfrow=c(1,1))
with(SCLoam, tapply(raw2^(-3.7/10), Field.CO3, mean, na.rm=T))

# Field texture is CL
summary(powerTransform(raw2~Field.CO3, data=ClayLoam))
boxcox(raw2^(7.7/10)~Field.CO3, data=ClayLoam)
contrasts(ClayLoam$Field.CO3) <- contr.treatment(5)
clayloam.lm02 <- lm(raw2^(7.7/10)~Field.CO3, data=ClayLoam)
anova(clayloam.lm02)
summary(clayloam.lm02)
par(mfrow=c(2,2))
plot(clayloam.lm02)
par(mfrow=c(1,1))
with(ClayLoam, tapply(raw2^(7.7/10), Field.CO3, mean, na.rm=T))

# Field texture is C
summary(powerTransform(raw2~Field.CO3, data=Clay))
boxcox(raw2^(11.4/10)~Field.CO3, data=Clay)
contrasts(Clay$Field.CO3) <- contr.treatment(5)
clay.lm02 <- lm(raw2^(11.4/10)~Field.CO3, data=Clay)
anova(clay.lm02)
summary(clay.lm02)
par(mfrow=c(2,2))
plot(clay.lm02)
par(mfrow=c(1,1))
with(Clay, tapply(raw2^(11.4/10), Field.CO3, mean, na.rm=T))

# All Field textures 
summary(powerTransform(raw2~Field.CO3*Field.Texture, data=mallee))
boxcox(raw2^(3/10)~Field.CO3*Field.Texture, data=mallee)
contrasts(mallee$Field.CO3) <- contr.treatment(5)
contrasts(mallee$Field.Texture) <- contr.treatment(7)
alltext.lm02 <- lm(raw2^(3/10)~Field.CO3*Field.Texture, data=mallee)
anova(alltext.lm02)
summary(alltext.lm02)
par(mfrow=c(2,2))
plot(alltext.lm02)
par(mfrow=c(1,1))
ave_raw2 <- with(mallee, tapply(raw2, list(Field.Texture,Field.CO3), mean, na.rm=T))

# write to Excel file
write.xlsx(ave_raw2, file="D:/Laroona Consulting/MRC/data/Average RAWs.xlsx", sheetName="raw2", append=T)

#================ analyse for taw - 8 - 1500 kPa ======================
ggplot(tawSum, aes(x=Field.Texture, y=taw, fill=Field.CO3)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=taw-se, ymax=taw+se),  width=.2, position=position_dodge(.9)) + scale_fill_manual(values=blues)

# linear regression on each hand texture class
summary(powerTransform(taw~Field.CO3, data=Sands))
boxcox(taw^(19.2/10)~Field.CO3, data=Sands)
contrasts(Sands$Field.CO3) <- contr.treatment(3)
sand.lm03 <- lm(taw^(19.2/10)~Field.CO3, data=Sands)
anova(sand.lm03)
summary(sand.lm03)
par(mfrow=c(2,2))
plot(sand.lm03)
par(mfrow=c(1,1))
with(Sands, tapply(taw^(19.2/10), Field.CO3, mean, na.rm=T))

# Field texture is LS
summary(powerTransform(taw~Field.CO3, data=LSand))
boxcox(taw^(8.2/10)~Field.CO3, data=LSand)
contrasts(LSand$Field.CO3) <- contr.treatment(4)
loamysand.lm03 <- lm(taw^(8.2/10)~Field.CO3, data=LSand)
anova(loamysand.lm03)
summary(loamysand.lm03)
par(mfrow=c(2,2))
plot(loamysand.lm03)
par(mfrow=c(1,1))
with(LSand, tapply(taw^(8.2/10), Field.CO3, mean, na.rm=T))

# Field texture is SL
summary(powerTransform(taw~Field.CO3, data=SandL))
boxcox(taw^(-0.13/10)~Field.CO3, data=SandL)
contrasts(SandL$Field.CO3) <- contr.treatment(5)
sandyloam.lm03 <- lm(taw^(-0.13/10)~Field.CO3, data=SandL)
anova(sandyloam.lm03)
summary(sandyloam.lm03)
par(mfrow=c(2,2))
plot(sandyloam.lm03)
par(mfrow=c(1,1))
with(SandL, tapply(taw^(-0.13/10), Field.CO3, mean, na.rm=T))

# Field texture is LSCL
contrasts(LSCLoam$Field.CO3) <- contr.treatment(5)
summary(powerTransform(taw~Field.CO3, data=LSCLoam))
boxcox(taw^(6.4/10)~Field.CO3, data=LSCLoam)
lscl.lm03 <- lm(taw^(6.4/10)~Field.CO3, data=LSCLoam)
anova(lscl.lm03)
summary(lscl.lm03)
par(mfrow=c(2,2))
plot(lscl.lm03)
par(mfrow=c(1,1))
with(LSCLoam, tapply(taw^(6.4/10), Field.CO3, mean, na.rm=T))

# Field texture is SCL
summary(powerTransform(taw~Field.CO3, data=SCLoam))
boxcox(taw^(-3.6/10)~Field.CO3, data=SCLoam)
contrasts(SCLoam$Field.CO3) <- contr.treatment(5)
scloam.lm03 <- lm(taw^(-3.6/10)~Field.CO3, data=SCLoam)
anova(scloam.lm03)
summary(scloam.lm03)
par(mfrow=c(2,2))
plot(scloam.lm03)
par(mfrow=c(1,1))
with(SCLoam, tapply(taw^(-3.6/10), Field.CO3, mean, na.rm=T))

# Field texture is CL
summary(powerTransform(taw~Field.CO3, data=ClayLoam))
boxcox(taw^(-8.8/10)~Field.CO3, data=ClayLoam)
contrasts(ClayLoam$Field.CO3) <- contr.treatment(5)
clayloam.lm03 <- lm(taw^(-8.8/10)~Field.CO3, data=ClayLoam)
anova(clayloam.lm03)
summary(clayloam.lm03)
par(mfrow=c(2,2))
plot(clayloam.lm03)
par(mfrow=c(1,1))
with(ClayLoam, tapply(taw^(-8.8/10), Field.CO3, mean, na.rm=T))

# Field texture is C
summary(powerTransform(taw~Field.CO3, data=Clay))
boxcox(taw^(-3.9/10)~Field.CO3, data=Clay)
contrasts(Clay$Field.CO3) <- contr.treatment(5)
clay.lm03 <- lm(taw^(-3.9/10)~Field.CO3, data=Clay)
anova(clay.lm03)
summary(clay.lm03)
par(mfrow=c(2,2))
plot(clay.lm03)
par(mfrow=c(1,1))
with(Clay, tapply(taw, Field.CO3, mean, na.rm=T))

# All Field textures 
summary(powerTransform(taw~Field.CO3*Field.Texture, data=mallee))
boxcox(taw^(4.6/10)~Field.CO3*Field.Texture, data=mallee)
contrasts(mallee$Field.CO3) <- contr.treatment(5)
contrasts(mallee$Field.Texture) <- contr.treatment(7)
alltext.lm03 <- lm(taw^(4.6/10)~Field.CO3*Field.Texture, data=mallee)
anova(alltext.lm03)
summary(alltext.lm03)
par(mfrow=c(2,2))
plot(alltext.lm03)
par(mfrow=c(1,1))
ave_taw <- with(mallee, tapply(taw, list(Field.Texture,Field.CO3), mean, na.rm=T))

# write to Excel file
write.xlsx(ave_taw, file="D:/Laroona Consulting/MRC/data/Average RAWs.xlsx", sheetName="taw", append=T)
