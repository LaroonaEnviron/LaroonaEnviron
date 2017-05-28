require(xlsx)
require(RColorBrewer)
blues <- colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE) #colorRampPalette(c("lightblue","navyblue"))
browns <- brewer.pal(9, "YlOrBr")[c(1,2,4,6,7)]

# read in major soil data file
(filein1 <- choose.files("*.xlsx"))
soils <- read.xlsx(filein1, 1, colClasses=c("integer","numeric","numeric","numeric","numeric","integer","integer","numeric","numeric","numeric","numeric","numeric","character","character","character","character","character","character", "character"), colIndex=c(1:20), stringsAsFactors=FALSE)
str(soils)

# convert State, Location, Texture, Field.Texture, Field.CO3 & Site from character to factor in dataframe soils
soils$State <- factor(soils$State)
soils$Location <- factor(soils$Location)

# convert Texture, Field.Texture & Field.CO3 to ordered factors
soils$Texture <- factor(soils$Texture, levels=c("SAND","LS","SL","LOAM","SCL","CL","CLAY"), ordered=TRUE)
soils$Field.Texture <- factor(soils$Field.Texture, levels=c("S","LS","SL","L","LSCL","SCL","CL","C"), ordered=TRUE)
soils$Field.CO3 <- factor(soils$Field.CO3, levels=,c("N","S","M","H","V"), ordered=TRUE)

soils$Region <- factor(soils$Region)
str(soils)

head(soils, 50)

# Use soil names as row names
rownames(soils) <- soils[,20]

# remove No and name
soils <- soils[, -c(1, 20)]

# calculate total available water (totaw) and readily available water rdyaw
soils$totaw <- with(soils, (KPa8 - KPa1500)*1000)
soils$rdyaw <- with(soils, (KPa8 - KPa60)*1000)

str(soils)
head(soils[ , c(1:11, 19,20)])
summary(soils)

require(ggplot2)
theme_set(theme_bw())

p1 <- ggplot(data=soils, aes(y=Clay, x=upper, colour=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green')) + geom_point() + facet_grid(Region~Field.Texture) + ylab("Clay (%)") + xlab("Depth (cm)") + ggtitle("Percent Clay")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa8*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Field.Texture)  + xlab("Clay (%)") + ylab("mm/m at 8 kPa") + ggtitle("mm of Water at 8 kPa") 
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa40*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Field.Texture)  + xlab("Clay (%)") + ylab("mm/m at  40kPa")  + ggtitle("mm of Water at 40 kPa")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa60*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Field.Texture)  + xlab("Clay (%)") + ylab("mm/m at 60 kPa")  + ggtitle("mm/m of wtasre at 60 kPa")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa1500*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Field.Texture)  + xlab("Clay (%)") + ylab("mm/m at 1500 kPa")  + ggtitle("mm/m of Water at 1500 kPa")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa1500*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + xlab("Clay (%)") + ylab("mm/m at 1500 kPa")
p1 + ggtitle("Percent Clay") + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, (KPa8-KPa1500)*1000,  color=Field.Texture)) + scale_color_manual(values=c('S'='red','LS'='salmon','SL'='brown', 'L'='darkgreen', 'LSCL'='gold', 'SCL'='blue', 'CL'='navyblue', 'C'='cyan')) + geom_point() + xlab("Clay (%)") + ylab("TAW mm/m")  + ggtitle("Available Water mm/m  by texture grade")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

p1 <- ggplot(data=soils, aes(Clay, (KPa8-KPa1500)*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='darkgreen')) + geom_point() + facet_wrap(~Field.Texture)  + xlab("Clay (%)") + ylab("TAW mm/m")  + ggtitle("Available Water mm/m  by texture grade by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(upper, Clay, color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Texture) + ylab("Clay (%)") + xlab("Depth (cm)")  +  ggtitle("% Clay  by depth by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa8*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green')) + geom_point() + facet_wrap(~Texture)  + xlab("Clay (%)") + ylab("mm/m at 8 kPa")  +  ggtitle("mm/m at 8 kPa by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="red"))

p1 <- ggplot(data=soils, aes(Clay, KPa40*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Texture)  + xlab("Clay (%)") + ylab("mm/m at  40kPa")   +  ggtitle("mm/m at 40kPa by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

p1 <- ggplot(data=soils, aes(Clay, KPa60*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Texture)  + xlab("Clay (%)") + ylab("mm/m at 60 kPa")  +  ggtitle("mm/m at 60kPa by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

p1 <- ggplot(data=soils, aes(Clay, KPa1500*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green'))  + geom_point() + facet_wrap(~Texture)  + xlab("Clay (%)") + ylab("mm/m at 1500 kPa")  +  ggtitle("Moisture (mm)/m soil at 1500kPa by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

p1 <- ggplot(data=soils, aes(Clay, (KPa8-KPa1500)*1000,  color=Region)) + scale_color_manual(values=c('Barossa'='red','Riverland'='blue','Sunraysia'='green')) + geom_point() + facet_wrap(~Texture)  + xlab("Clay (%)") + ylab("TAW mm/m")  +  ggtitle("Available Moisture (mm)/m soil between 8 - 1500kPa by Region")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

require(splines)
p1 <- ggplot(data=soils, aes(Sand, (KPa8-KPa1500)*1000, color=Field.CO3)) + scale_color_manual(values=c('N'='brown','S'='orange','M'='blue','H'='green','V'='red'))  + geom_point() + geom_smooth(method='lm', formula= y~ns(x,4), se=F) + ylab("TAW mm/m") + xlab("% Sand")   +  ggtitle("Available Moisture (mm)/m soil between 8 - 1500kPa by Carbonate Class")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

p1 <- ggplot(data=soils, aes(Field.CO3, (KPa8-KPa40)*1000, color=Field.CO3)) + scale_color_manual(values=c('N'='brown','S'='orange','M'='blue','H'='green','V'='red'))  + geom_boxplot() + facet_wrap(~Field.Texture) + ylab("Readily available water mm/m") + xlab("1N HCl score")   +  ggtitle("Available Moisture (mm)/m soil between 8 - 40kPa by Carbonate Class")
p1 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20, color="blue"))

p1 <- ggplot(data=soils, aes(x=totaw, fill=Region)) + geom_histogram(binwidth=10)  + scale_color_manual(values=c('Barossa'='brown','Riverland'='orange','Sunraysia'='green'))+ facet_grid(~Region)
p1

p1 <- ggplot(data=soils, aes(x=rdyaw, fill=Region)) + geom_histogram(binwidth=5)  + scale_fill_manual(values=c("red","blue","green"))
p1
ggplot(data=soils, aes(x=KPa8*1000, y=..density..)) + geom_histogram(fill="orange", color="gray", binwidth=20) + geom_density()

ftable(soils[c("Texture","Field.CO3")])
ftable(soils[c("Field.Texture","Field.CO3")])

tabs.40 <- with(soils, tapply((KPa8-KPa40)*1000, list(Field.Texture,Field.CO3), mean))
names(dimnames(tabs.40)) <- list("Texture Grade", "Carbonate Class")
round(tabs.40, 1)

tabs.60 <- with(soils, tapply((KPa8-KPa60)*1000, list(Field.Texture,Field.CO3), mean))
names(dimnames(tabs.60)) <- list("Texture Grade", "Carbonate Class")
round(tabs.60, 1)

tabs.1500 <- with(soils, tapply((KPa8-KPa1500)*1000, list(Field.Texture, Field.CO3), mean))
names(dimnames(tabs.1500)) <- list("Texture Grade", "   Carbonate Class")
round(tabs.1500, 1)

# subset mallee only soils
mallee <- subset(soils, Region=="Riverland" | Region=="Sunraysia")
mallee$Region <- factor(mallee$Region)
mallee$Field.Texture <- factor(mallee$Field.Texture)
str(soils)
str(mallee)
summary(mallee)

mtabs.1500 <- with(mallee, tapply((KPa8-KPa1500)*1000, list(Field.Texture, Field.CO3), mean, na.rm=T))
names(dimnames(mtabs.1500)) <- list("Texture Grade", "Carbonate Class")
round(mtabs.1500, 1)

stabs.1500 <- with(mallee, tapply((KPa8-KPa1500)*1000, list(Field.Texture, Field.CO3), sd, na.rm=T))
names(dimnames(stabs.1500)) <- list("Texture Grade", "Carbonate Class")
round(stabs.1500, 1)

ctabs.1500 <- with(mallee, tapply((KPa8-KPa1500)*1000, list(Field.Texture, Field.CO3), length))
ctabs.1500
setabs.1500 <- stabs.1500/ctabs.1500
round(setabs.1500,2)

require(reshape2)
taw <- melt(mtabs.1500, varnames=c("texture","carbonate"), value.name="taw")
taw.se <- melt(setabs.1500, varnames=c("texture","carbonate"), value.name="se")
taw <- merge(taw,taw.se)
str(taw)

p2 <- ggplot(taw, aes(x=texture, y=taw, fill=carbonate)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=taw-se, ymax=taw+se),  width=.2, position=position_dodge(.9)) + xlab("carbonate class") + ylab("Total Available Water (mm/m)")    +  ggtitle("Available Moisture (mm)/m soil by Carbonate Class")
p2 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20))

                  
mallee.60 <- with(mallee, tapply((KPa8-KPa60)*1000, list(Field.Texture,Field.CO3), mean))
mallee.60sd <- with(mallee, tapply((KPa8-KPa60)*1000, list(Field.Texture,Field.CO3), sd))
mallee.60n <- with(mallee, tapply((KPa8-KPa60)*1000, list(Field.Texture,Field.CO3), length))
round(mallee.60, 1)
round(mallee.60sd, 1)
mallee.60se <- mallee.60sd/mallee.60n
round(mallee.60se, 2)

textnames <-  unlist(dimnames(tabs.1500)[1])
textnames[1]
barplot(mallee.60, col=blues(8), beside=TRUE, legend.text=T)

Raw <- melt(mallee.60, varnames=c("texture","carbonate"), value.name="Raw")
Raw.se <- melt(mallee.60se, varnames=c("texture","carbonate"), value.name="se")
Raw <- merge(Raw,Raw.se)
str(Raw)

p2 <- ggplot(Raw, aes(x=carbonate, y=Raw, fill=texture)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Raw-se, ymax=Raw+se),  width=.2, position=position_dodge(.9))  +  ggtitle("Readily available water (mm)/m soil by Carbonate Class")
p2 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20))

p2 <- ggplot(Raw, aes(x=texture, y=Raw, fill=carbonate)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Raw-se, ymax=Raw+se),  width=.2, position=position_dodge(.9))  +  ggtitle("Readily available water (mm)/m soil by texture grade")
p2 + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20))

require(car)
summary(powerTransform((KPa1500*1000)~Field.Texture*Field.CO3, data=mallee))
boxcox((KPa1500)^(3/10)*1000~Field.Texture*Field.CO3, data=mallee)
options(contrasts=c("contr.treatment","contr.helmert"))
mallee.lm01 <- lm((KPa1500*1000)^(3/10)~Field.Texture*Field.CO3-1, data=mallee)
anova(mallee.lm01)
summary(mallee.lm01)
par(mfrow=c(2,2))
plot(mallee.lm01)
par(mfrow=c(1,1))

summary(powerTransform(((KPa8-KPa40)*1000)~Field.Texture*Field.CO3, data=mallee))
boxcox(((KPa8-KPa40)*1000)^(4/10)~Field.Texture*Field.CO3, data=mallee)
options(contrasts=c("contr.treatment","contr.helmert"))
mallee.lm01 <- lm(((KPa8-KPa40)*1000)^(4/10)~Field.Texture*Field.CO3, data=mallee)
anova(mallee.lm01)
summary(mallee.lm01)
par(mfrow=c(2,2))
plot(mallee.lm01, col=mallee$Field.CO3, pch=16)
par(mfrow=c(1,1))

awc_1 <- with(mallee, tapply((KPa8-KPa1500)*1000, list(Field.Texture, Field.CO3), mean))
names(dimnames(awc_1)) <- list("Texture Grade", "Carbonate Class")
round(awc_1, 1)
barplot(t(awc_1), beside=TRUE, col=browns, legend.text=TRUE)

awc_2 <- with(mallee, tapply((KPa8-KPa40)*1000, list(Field.CO3, Field.Texture), mean))
names(dimnames(awc_2)) <- list("Carbonate Class", "Texture Grade")
round(awc_2, 1)

awc_3 <- with(soils, tapply((KPa8-KPa60)*1000, list(Field.CO3, Field.Texture), mean))
round(awc_3, 1)

barplot(awc_2, beside=TRUE, legend.text=TRUE, col=browns, xlab="Texture Grades", ylab="Available Water (mm/m)", border="black",  args.legend = list(x = 41, y=66))
text(40, 71, labels="Fine\n Earth\n Carbonate\n Classes" )
title(main="Effect of Fine Earth Carbonates on Available Water Content\n Mallee soils")

# with soils dataframe
barplot(awc_3, beside=TRUE, legend.text=TRUE, col=browns, xlab="Texture Grades", ylab="Available Water (mm/m)", border="black",  args.legend = list(x = 46, y=75))
text(45, 80, labels="Fine\n Earth\n Carbonate\n Classes" )
title(main="Effect of Fine Earth Carbonates on Available Water Content\n Mallee, Barossa and Adelaide Hills soils")

Raw <- with(mallee, tapply((KPa8-KPa60)*1000, list(Field.CO3, Field.Texture), mean))
names(dimnames(Raw)) <- list("Reaction", "Texture Grade")
round(Raw, 1)
barplot(Raw, beside=TRUE, legend.text=TRUE, args.legend = list(x = 2.5, y=70), col=browns, xlab="Texture Grades", ylab="Available Water (mm/m)", border="blue")
text(1.5,77, labels="Fine\n Earth\n Carbonate\n Classes" )
title(main="Effect of Fine Earth Carbonates on Readily Available Water Content\n Mallee soils")

require(rgl)
z <- with(mallee, (KPa8-KPa60)*1000)
x <- mallee$Field.Texture
y <- mallee$Field.CO3
plot3d(x, y, z, type="s", size=1, xlab="Texture Grade", ylab="Carbonate Reaction Class", zlab="Available Water (mm/m)", col=as.integer(y))

x <- mallee$Sand
c <- mallee$Carbonate
with(mallee, plot3d(Sand, Carbonate, (KPa8-KPa60)*1000, type="s", size=1, xlab="Sand (%)", ylab="Carbonate (%)", zlab="Available Water (mm/m)", col=as.integer(Field.CO3)))

qplot(x, z, data=mallee, colour=y, geom=c("point", "smooth"), se=F)

# Basic plot
v <- ggplot(mallee, aes(Sand, Carbonate, z = Raw))
v + stat_contour()

mallee$raw1 <- with(mallee, (KPa8-KPa40)*1000)
mallee$raw2 <- with(mallee, (KPa8-KPa60)*1000)
mallee$taw  <- with(mallee, (KPa8-KPa1500)*1000)
str(mallee)
write.csv(mallee, file="Malleesoils.csv")

# calculate distance matrix
head(mallee[, c(3,5:7,10:14,19)], 30)
head(mallee[, c(3,5:7,12,15,19)], 30)

d <- dist(mallee[, c(3,5:7,12,15,19)])
max(d);min(d)
scaled <- d/max(d)
min(scaled)
par(mfrow=c(1,3))
plot(hclust(scaled, method="complete"),labels=mallee$No, ylab="Scaled Distance", cex=0.5)
plot(hclust(scaled, method="centroid"),labels=mallee$No, ylab="Scaled Distance", cex=0.5)
plot(hclust(scaled, method="median"),labels=mallee$No, ylab="Scaled Distance", cex=0.5)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(hclust(scaled, method="complete"),labels=mallee$No, ylab="Scaled Distance", cex=0.5)
plot(hclust(d, method="complete"),labels=mallee$No, ylab="Scaled Distance", cex=0.5)
par(mfrow=c(1,1))


d_man <- dist(mallee_1, method="manhattan")
d_euc <- dist(mallee_1, method="euclidian")
d_max <- dist(mallee_1, method="maximum")

max(d_man);min(d_man)
max(d_euc);min(d_euc)
max(d_max);min(d_max)

par(mfrow=c(1,3))
plot(hclust(d_man, method="complete"),labels=mallee$No, ylab="Scaled Distance", cex=0.5, main="Complete Manhattan")
plot(hclust(d_euc, method="complete"),labels=mallee$No, ylab="Scaled Distance", cex=0.5, main="Complete Euclidian")
plot(hclust(d_max, method="complete"),labels=mallee$No, ylab="Scaled Distance", cex=0.5, main="Complete Maximum")
par(mfrow=c(1,1))

n <- nrow(mallee)
mallee_1 <- with(mallee, cbind(Clay, Sand, upper, lower, as.integer(Field.Texture), as.integer(Field.CO3), KPa8, KPa60, raw2))
mallee_1 <- as.data.frame(mallee_1)
names(mallee_1)[5:6] <- c("Texture","CO3")
str(mallee_1)

wss <- numeric(0)
for (i in 1:14) {
   W <- sum(kmeans(mallee_1$raw2, i)$withinss)
   wss <- c(wss, W)
   }

delwss <- 1
for (i in 2:14) {
  delwss[i] <- (wss[i-1]-wss[i])/wss[1]
  }
  
par(mfrow=c(1,2))
plot(1:14, wss, type="l", xlab="Number of Groups", ylab="Within groups sum of squares", lwd=2)
plot(1:14, delwss*100, type="l", xlab="Number of Groups", ylab="Change in Within groups sum of squares (%)", lwd=2)
par(mfrow=c(1,1))

mallee.kmean <- kmeans(mallee_1, 6)
mallee.kmean
raw.grps <- lapply(1:6, function(nc) apply(mallee_1[mallee.kmean$cluster==nc,],2, mean))
table(mallee.kmean$cluster)

str(raw.grps)

plot(mallee_1$Sand, mallee_1$raw2, pch=16, col=mallee.kmean$cluster+1)
points(mallee.kmean$centers[,2], mallee.kmean$centers[,9], pch=letters[1:7], col=2:11, cex=3)
