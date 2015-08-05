require(DAAG)
# replace NaN with NA
pumphrs$hrs <- ifelse(is.nan(pumphrs$hrs), NA, pumphrs$hrs)

str(pumphrs)

caissons <- unique(pumphrs$caisson)
for (i in 1:length(caissons)) {
    print(xyplot(time~Date, groups=Pump, data=subset(pumphrs, caisson==caissons[i]), 
    type='l', col=c('red','blue'), xlab="Year", ylab="Pump Hours", 
    main=paste("Caisson ", caissons[i])))
    pause()
  }

caissons <- unique(use$caisson)
for (i in 1:length(caissons)) {
    print(xyplot(use~Date, groups=tariff, data=subset(use, caisson==caissons[i]), 
    type='l', col=c('red','blue'), xlab="Year", ylab="Electricity Use(kwh)",    
    main=paste("Caisson ", caissons[i])))
    pause()
  }

caissons <- unique(Vol$caisson)
for (i in 1:length(caissons)) {
    print(xyplot(Vol~Date, groups=subcaiss, data=subset(Vol, caisson==caissons[i]), 
    type='l', col=c('red','blue'), xlab="Year", ylab="Volume pumped (Ml)",    
    main=paste("Caisson ", caissons[i])))
    pause()
  }
  
# plot year vs vol
qplot(Date, vol, data=Vol, facets=~caisson, col=subcaiss, geom='line')

#---------------------------------------------------------------------
# aggregate pumphrs by year and caisson
hrsyr <- with(pumphrs, aggregate(hrs, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(hrsyr) <- c('year','caisson','cumhours')
hrsyr$year <- as.integer(hrsyr$year)
head(hrsyr, 20)

timeyr <- with(pumphrs, aggregate(time, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(timeyr) <- c('year','caisson','hours')
timeyr$year <- as.integer(timeyr$year)
head(timeyr, 20)

# aggregate use by year and caisson
Kwyear <- with(use, aggregate(use, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(Kwyear) <- c('year','caisson','Kw')
Kwyear$year <- as.integer(kwhyear$year)
head(Kwyear, 20)

nrow(timeyr); nrow(Kwyear)

#  aggregate use by year and caisson
annVol <- with(Vol, aggregate(Vol, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(annVol) <- c('year','caisson','Ml')
annVol$year <- as.integer(annVol$year)
annVol$caisson <- as.numeric(annVol$caisson)
annVol

nrow(annVol)

# merge yearly pump hours and electricity use
caissdat <- data.frame(hrsyr[,], timeyr[,3], Kwyear[,3])
names(caissdat)[4:5] <- c('hours','cumKw')
caissdat$caisson <- factor(caissdat$caisson)
head(caissdat)

# merge yearly pump hours, electricity use and volume pumped
df1 <- merge(annVol, timeyr)
df1 <- df1[order(df1$caisson, df1$year),]
head(df1, 50)

df2 <- merge(annVol, kwhyear)
df2 <- df2[order(df2$caisson, df2$year),]
head(df2, 50)

df3 <- merge(df1, df2)
df3 <- df3[order(df3$caisson, df3$year),]
df3 <- subset(df3, Ml > 0)
head(df3, 50)
