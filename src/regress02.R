require(RColorBrewer)
require(car)
require(xlsx)

# plot out available water holding capacities
# summarize available water categories
raw1Sum <- summarySE(mallee, "raw1", c("Texture","Field.CO3"))
raw2Sum <- summarySE(mallee, "raw2", c("Texture","Field.CO3"))
tawSum <- summarySE(mallee, "taw", c("Texture","Field.CO3"))

# add to summary dataframe missing factors for later consistent plotting
raw1Sum <- addrows(raw1Sum)
raw2Sum <- addrows(raw2Sum)
tawSum <- addrows(tawSum)
  
# define color ranges
browns <- brewer.pal(9, "YlOrBr")
blues <- brewer.pal(9, "Blues")[c(4:8)]
greys <- brewer.pal(9, "Greys")[c(3:7)]

# plot bar graph with +/-se with ggplot
ggplot(raw1Sum, aes(x=Texture, y=raw1, fill=Field.CO3)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=raw1-se, ymax=raw1+se),  width=.2, position=position_dodge(.9)) + scale_fill_manual(values=greys)

# barplot
barplot(raw1Sum$raw1, beside=TRUE, legend.text=TRUE, col=browns, xlab="Texture Grades", ylab="Available Water (mm/m)", border="black")
text(35,70, labels="Fine\n Earth\n Carbonate\n Classes" )
title(main="Effect of Fine Earth Carbonates on Available Water Content\n Mallee Soils")

#================ analyse for raw2 - 8 - 60 kPa ======================
ggplot(raw2Sum, aes(x=Texture, y=raw2, fill=Field.CO3)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=raw2-se, ymax=raw2+se),  width=.2, position=position_dodge(.9)) + scale_fill_manual(values=browns)

# linear regression on each hand texture class
sands$Field.CO3 <- factor(sands$Field.CO3)
summary(powerTransform(raw2~Field.CO3, data=sands))
boxcox(raw2^(3.5/10)~Field.CO3, data=sands)
contrasts(sands$Field.CO3) <- contr.treatment(4)
sand.lm02 <- lm(raw2^(3.5/10)~Field.CO3, data=sands)
anova(sand.lm02)
summary(sand.lm02)
par(mfrow=c(2,2))
plot(sand.lm02)
par(mfrow=c(1,1))
with(sands, tapply(raw2, Field.CO3, mean, na.rm=T))

# Field texture is LS
summary(powerTransform(raw2~Field.CO3, data=lsands))
boxcox(raw2^(-1.4/10)~Field.CO3, data=lsands)
contrasts(lsands$Field.CO3) <- contr.treatment(5)
loamysand.lm02 <- lm(raw2^(-1.4/10)~Field.CO3, data=lsands)
anova(loamysand.lm02)
summary(loamysand.lm02)
par(mfrow=c(2,2))
plot(loamysand.lm02)
par(mfrow=c(1,1))
with(lsands, tapply(raw2, Field.CO3, mean, na.rm=T))

# Field texture is SL
sandyl <- subset(mallee, Texture=="SL")
sandyl$Field.CO3 <- factor(sandyl$Field.CO3)
summary(powerTransform(raw2~Field.CO3, data=sandyl))
boxcox(raw2^(-0.8/10)~Field.CO3, data=sandyl)
contrasts(sandyl$Field.CO3) <- contr.treatment(5)
sandyloam.lm02 <- lm(raw2^(-0.8/10)~Field.CO3, data=sandyl)
anova(sandyloam.lm02)
summary(sandyloam.lm02)
par(mfrow=c(2,2))
plot(sandyloam.lm02)
par(mfrow=c(1,1))
with(sandyl, tapply(raw2, Field.CO3, mean, na.rm=T))

# Field texture is Loam
loam <- subset(mallee, Texture=="LOAM")
loam$Field.CO3 <- factor(loam$Field.CO3)

contrasts(loam$Field.CO3) <- contr.treatment(5)
summary(powerTransform(raw2~Field.CO3, data=loam))
boxcox(raw2^(1.5/10)~Field.CO3, data=loam)
loam.lm02 <- lm(raw2^(1.5/10)~Field.CO3, data=loam)
anova(loam.lm02)
summary(loam.lm02)
par(mfrow=c(2,2))
plot(loam.lm02)
par(mfrow=c(1,1))

with(loam, tapply(raw2, Field.CO3, mean, na.rm=T))

# Field texture is SCL
scl <- subset(mallee, Texture=='SCL')
scl$Field.CO3 <- factor(scl$Field.CO3)

summary(powerTransform(raw2~Field.CO3, data=scl))
boxcox(raw2^(-1.9/10)~Field.CO3, data=scl)
contrasts(scl$Field.CO3) <- contr.treatment(5)
scl.lm02 <- lm(raw2^(-1.9/10)~Field.CO3, data=scl)
anova(scl.lm02)
summary(scl.lm02)
par(mfrow=c(2,2))
plot(scl.lm02)
par(mfrow=c(1,1))
with(scl, tapply(raw2, Field.CO3, mean, na.rm=T))

# Field texture is CL
cl <- subset(mallee, Texture=='CL')
cl$Field.CO3 <- factor(cl$Field.CO3)

summary(powerTransform(raw2~Field.CO3, data=cl))
boxcox(raw2^(4.7/10)~Field.CO3, data=cl)
contrasts(cl$Field.CO3) <- contr.treatment(5)
cl.lm02 <- lm(raw2^(4.7/10)~Field.CO3, data=cl)
anova(cl.lm02)
summary(cl.lm02)
par(mfrow=c(2,2))
plot(cl.lm02)
par(mfrow=c(1,1))
with(cl, tapply(raw2, Field.CO3, mean, na.rm=T))

# Field texture is C
clay <- subset(mallee, Texture=='CLAY')
clay$Field.CO3 <- factor(clay$Field.CO3)

summary(powerTransform(raw2~Field.CO3, data=clay))
boxcox(raw2^(26.9/10)~Field.CO3, data=clay)
contrasts(clay$Field.CO3) <- contr.treatment(4)
clay.lm02 <- lm(raw2^(26.9/10)~Field.CO3, data=clay)
anova(clay.lm02)
summary(clay.lm02)
par(mfrow=c(2,2))
plot(clay.lm02)
par(mfrow=c(1,1))
with(clay, tapply(raw2, Field.CO3, mean, na.rm=T))

# All Field textures 
summary(powerTransform(raw2~Field.CO3*Texture, data=mallee))
boxcox(raw2^(4.2/10)~Field.CO3*Texture, data=mallee)
contrasts(mallee$Field.CO3) <- contr.treatment(5)
contrasts(mallee$Texture) <- contr.treatment(7)
AllText.lm02 <- lm(raw2^(4.2/10)~Field.CO3*Texture, data=mallee)
anova(AllText.lm02)
summary(AllText.lm02)
par(mfrow=c(2,2))
plot(AllText.lm02)
par(mfrow=c(1,1))
Ave_raw2 <- with(mallee, tapply(raw2, list(Texture,Field.CO3), mean, na.rm=T))
floor(Ave_raw2)
ceiling(Ave_raw2)
round(Ave_raw2,0)
