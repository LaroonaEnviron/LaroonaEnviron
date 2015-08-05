# plot past estimates of irrigation pumped volumes & accessions
require(DAAG)

# plot out megalitres vs power consumption and hours pumped

theme_set(theme_bw())
qplot(kwh, Ml, data=df3, facets=.~caisson)

p <- ggplot(df3, aes(year, Ml)) + geom_line()
p + facet_wrap(~caisson, scales="free_y")

p <- ggplot(df3, aes(kwh, Ml)) + geom_point()
p + facet_wrap(~caisson, scales="free")

p <- ggplot(df3, aes(hours, Ml)) + geom_point()
p + facet_wrap(~caisson, scales="free")

for (n in 1:6) {
  print(subset(df3, caisson==n))
  pause()
  }

# linear models of vol vs power consumption and hours pumped
# Caisson 1
# fit linear model of kwh vs hrs pumped
df3$caisson <- factor(df3$caisson)
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==1 & year>2005)))

caiss01.lm01 <- lm(Ml~kwh-1, data=subset(df3, caisson==1 & year>2005))
anova(caiss01.lm01)
summary(caiss01.lm01)
par(mfrow=c(2,2))
plot(caiss01.lm01)
par(mfrow=c(1,1))

with(subset(df3, caisson==1 & year>2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss01.lm01), lty=2, col="red")

# update - add sqr term
caiss01.lm02 <- update(caiss01.lm01, .~.+I(kwh*kwh))
anova(caiss01.lm02, caiss01.lm01)
anova(caiss01.lm02)
summary(caiss01.lm02)
par(mfrow=c(2,2))
plot(caiss01.lm02)
par(mfrow=c(1,1))

with(subset(df3, caisson==1 & year>2005), plot(kwh, Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
new <- data.frame(kwh = seq(0, 12000, 100))
lines(new$kwh, predict(caiss01.lm02, new), lty=2, col='red')

# caisson 2
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==2 & year>2005)))

caiss02.lm01 <- lm(Ml~kwh-1, data=subset(df3, caisson==2 & year>2005))
anova(caiss02.lm01)
summary(caiss02.lm01)
par(mfrow=c(2,2))
plot(caiss02.lm01)
par(mfrow=c(1,1))

with(subset(df3, caisson==2 & year>2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss02.lm01), lty=2, col="red")

# caisson 3
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==3 & year>2005)))

caiss03.lm01 <- lm(Ml~kwh-1, data=subset(df3, caisson==3 & year>2005))
anova(caiss03.lm01)
summary(caiss03.lm01)
par(mfrow=c(2,2))
plot(caiss03.lm01)
par(mfrow=c(1,1))

with(subset(df3, caisson==3 & year>2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss03.lm01), lty=2, col="red")

# caisson 4
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==4 & year>2005)))

caiss04.lm01 <- lm(Ml~kwh-1, data=subset(df3, caisson==4 & year>2005))
anova(caiss04.lm01)
summary(caiss04.lm01)
par(mfrow=c(2,2))
plot(caiss04.lm01)
par(mfrow=c(1,1))

with(subset(df3, caisson==4 & year>2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss04.lm01), lty=2, col="red")

# caisson 5
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==5 & year>2005)))

caiss05.lm01 <- lm(Ml~kwh-1, data=subset(df3, caisson==5 & year>2005))
anova(caiss05.lm01)
summary(caiss05.lm01)
par(mfrow=c(2,2))
plot(caiss05.lm01)
par(mfrow=c(1,1))

with(subset(df3, caisson==5 & year>2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss05.lm01), lty=2, col="red")

# caisson 6
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==6 & year>2005)))

caiss06.lm01 <- lm(Ml~kwh-1, data=subset(df3, caisson==6 & year>2005))
anova(caiss06.lm01)
summary(caiss06.lm01)
par(mfrow=c(2,2))
plot(caiss06.lm01)
par(mfrow=c(1,1))

with(subset(df3, caisson==6 & year>2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss06.lm01), lty=2, col="red")

# caisson 4 before 2006
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==4 & year<=2005)))

caiss04b.lm02 <- lm(Ml~kwh-1, data=subset(df3, caisson==4 & year<=2005))
anova(caiss04b.lm02)
summary(caiss04b.lm02)
par(mfrow=c(2,2))
plot(caiss04b.lm02)
par(mfrow=c(1,1))

with(subset(df3, caisson==4 & year<=2005), plot(kwh, Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
abline(a=0, b=coef(caiss04b.lm02), lty=2, col="red")

# caisson 5 --  before 2006
summary(powerTransform(Ml~kwh, data=subset(df3, caisson==5 & year<=2005)))

caiss05.lm02 <- lm(Ml~sqrt(kwh)-1, data=subset(df3, caisson==5 & year<=2005))
anova(caiss05.lm02)
summary(caiss05.lm02)
par(mfrow=c(2,2))
plot(caiss05.lm02)
par(mfrow=c(1,1))

with(subset(df3, caisson==5 & year<=2005), plot(kwh,Ml, pch=16, col='blue', xlab='Power Consumption (kilowatts)', ylab='Volume pumped (Ml)'))
new <- data.frame(kwh = seq(0, 2200, 100))
lines(new$kwh, predict(caiss05.lm02, new), lty=2, col='red')

# plot out all data 
sumps <- as.integer(df3[df3$caisson!="5",]$caisson)
xyplot(Ml~kwh, groups=sumps, data=subset(df3, caisson!="5"), pch=16,
  panel=function(x,y){
          panel.xyplot(x,y)
          panel.abline(lm(y~x))
          },
  pch=16,
  col=sumps+1
  )

with(subset(df3, year > 2003 & caisson !="5"), {
  print(summary(lm(Ml~kwh-1)))
  plot(kwh,Ml, col=sumps, pch=15+sumps,
  xlab="power consumption (kw)", ylab="Volume pumped (Ml)")
  abline(a=0, b=coef(lm(Ml~kwh-1)))
  legend("topleft", legend=c("caisson 1","caisson 2","caisson 3","caisson 4","caisson 6","fitted"), pch=c(16,17,18,19,20), col=15+sumps, lty=c(0,0,0,0,0,1))
  })

# use hours pumped
with(subset(df3, year > 2005), {
  print(summary(lm(vol~hours-1)))
  plot(hours,vol, col=as.integer(caisson), pch=16,
  xlab="Time pumping (hrs)", ylab="Volume pumped (Ml)")
  abline(a=0, b=coef(lm(vol~hours-1)))
  legend("topleft", legend=c("caisson 1","caisson 2","caisson 3","caisson 4","caisson 5","caisson 6","fitted"), pch=16, col=c(1:6), lty=c(0,0,0,0,0,0,1))
  })

# plot out pump hours and power consumption
# first by year then against each other
theme_set(theme_bw())

p <- ggplot(subset(df3, year > 2003 & caisson !="5"), aes(kwh, Ml)) + geom_point()
p + geom_smooth(method="lm")

# hours pumped
p <- ggplot(caissdat, aes(year, hours)) + geom_line()
p + facet_wrap(~caisson, scales="free_y", nrow=2)

# power consumption
p <- ggplot(caissdat, aes(year, cumKw)) + geom_line()
p + facet_wrap(~caisson, scales="free_y", nrow=2)

# hours pumped vs power consumption
p <- ggplot(caissdat, aes(cumKw, hours)) + geom_point()
p + facet_wrap(~caisson, scales="free")

# estimate volume pumped assuming a single regression line
caissdat$Ml <- with(caissdat, cumKw * 0.0124)

p <- ggplot(caissdat, aes(year, Ml)) + geom_line()
p + facet_wrap(~caisson, scales="free_y")

# sum over all caissons
CDS <- with(caissdat, aggregate(Ml, by=list(year),sum, na.rm=T))
names(CDS) <- c("year","Ml")
with(CDS, plot(Ml~year, type='l', col="blue", xlab="Year", ylab="CDS Volume (Ml)"))

# writeout to Excel spreadsheet derived datasets
file <- "derived.xlsx"
write.xlsx2(caissdat, file, sheetName="AllCaisson")
write.xlsx2(df3, file, sheetName="volumes", append=T)
write.xlsx2(CDS, file, sheetName="EstCDSVol", append=T)
