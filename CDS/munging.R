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

# aggregate pumphrs by month and caisson
pumphrs.agg <- with(pumphrs, aggregate(hrs, by=list(substr(Date, 1,7), caisson),sum, na.rm=T))
names(pumphrs.agg) <- c('month','caisson','hours')
pumphrs.agg$month <- as.Date(paste(pumphrs.agg$month,'01',sep='-'), format="%Y-%m-%d")
head(pumphrs.agg)

mnthhrs <- with(pumphrs, aggregate(time, by=list(substr(Date, 1,7), caisson),sum, na.rm=T))
head(mnthhrs, 20)

# aggregate use by month and caisson
mnthkwh <- with(use, aggregate(use, by=list(substr(Date, 1,7), caisson),sum, na.rm=T))
names(mnthkwh) <- c('month','caisson','hours')
mnthkwh$month <- as.Date(paste(mnthkwh$month,'01',sep='-'), format="%Y-%m-%d")
head(mnthkwh, 20)

#---------------------------------------------------------------------
# aggregate pumphrs by year and caisson
hrsyr <- with(pumphrs, aggregate(hrs, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(hrsyr) <- c('year','caisson','cumhours')
hrsyr$year <- as.integer(hrsyr$year)
head(hrsyr)

timeyr <- with(pumphrs, aggregate(time, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(timeyr) <- c('year','caisson','hours')
timeyr$year <- as.integer(timeyr$year)
head(timeyr, 20)

# aggregate use by year and caisson
kwhyear <- with(use, aggregate(use, by=list(substr(Date, 1,4), caisson),sum, na.rm=T))
names(kwhyear) <- c('year','caisson','hours')
kwhyear$year <- as.integer(kwhyear$year)
head(kwhyear, 20)

nrow(timeyr); nrow(kwhyear)

caissdat <- data.frame(hrsyr[,], timeyr[,3], kwhyear[,3])
names(caissdat)[4:5] <- c('hours','cumkwh')
head(caissdat)

theme_set(theme_bw())
qplot(hours, cumkwh, data=caissdat, facets=~caisson)
qplot(year, cumkwh, data=caissdat, facets=~caisson)

  