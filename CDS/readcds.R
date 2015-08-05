# read in cds data
require(xlsx)
require(tidyr)

# read in details of each CDS spreadsheet
detfile <- "RIT_CDS_details.xlsx" 
detail <- read.xlsx2(detfile, 1, header=T, stringsAsFactors=F, colClasses=c(rep("character",3), rep('numeric',3)))

#================ define functions ==========================
position <- function(X) {
  # Calculates the number of rows or columns in the EXCEL spreadsheet
  pos.1 <- match(substr(X,1,1), LETTERS)
  pos.2 <- match(substr(X,2,2), LETTERS)
  pos.1 * 26 + pos.2
  }

chsplit <- function(string) {
  # split a string into integer and character parts
  len <- length(string)
  caisson <- vector(length=len)
  pump <- vector(length=len)

  # iterate over number of strings  
  for (i in 1:len) {
    nch <- nchar(string[i])
    caisson[i] <- ifelse(nch==2, substr(string[i],1,1), substr(string[i],1,2))
    pump[i] <- ifelse(nch==2, substr(string[i],2,2), substr(string[i],3,3))
  }
  
  #browser()
  list(as.integer(caisson), pump)
  }
#========================== end definition ==============================
 
# read in the spreadsheet and extract hours and power consumption
ncols <- position(detail$Hours.Col)

for (i in 1:4) {
   (cat(i, detail$File[i],"\n"))
   pump <- read.xlsx(detail$File[i], 1, colIndex=1:ncols[i], endRow=30, header=T, stringsAsFactors=F, colClasses=c("character", rep("numeric", ncols[i]-1)))

  # convert from wide format to narrow
  hours <- gather(pump, Date, hrs, -DATE.)
  hours <- subset(hours, DATE. != "DP-1O7" & DATE. != "DP-167" & DATE. != "DP-170")
  
  # remove HRS- from DATE. variable values & rename to 'caisson'
  clist <- strsplit(hours$DATE., '-')
  caisson <- rapply(clist, function(x) tail(x,1))
  
  caissonList <- chsplit(caisson)
  
  caisson <- unlist(caissonList[1])
  Pump <- unlist(caissonList[2])
  
  hours <- data.frame(caisson, Pump, hours[,2:3])

  # extract numeric part of Date and convert to Date
  hours$Date <- as.character(hours$Date)
  hours$Date <- substr(hours$Date, 2, 6)
  hours$Date <- as.Date(as.integer(hours$Date), origin="1899-12-30", format="%Y-%m-%d")
  
  # store hours in appropriate date dataframe
  if(i==1) pumphrs <- hours else pumphrs <- rbind(pumphrs, hours)
  }

# plot time vs hours pumped
theme_set(theme_bw())
qplot(Date, hrs, data=pumphrs, color=Pump, facets=Pump~caisson, geom='line')

# calculate hours run
pumphrs$time[27:nrow(pumphrs)] <- with(pumphrs, hrs[27:nrow(pumphrs)] - hrs[1:(nrow(pumphrs)-26)])
  
# correct negative hours
ivec <- with(pumphrs, which(time < 0))
pumphrs[ivec,]
pumphrs$time[ivec] <- NA
qplot(Date, time, data=pumphrs, color=Pump, facets=~caisson, geom='line')

# sort data on caisson name
pumphrs.srt <- pumphrs[order(pumphrs$caisson, pumphrs$Pump),]
head(pumphrs.srt, 200)
tail(pumphrs.srt, 200)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# read in power consumption and volumetric data
ncols <- position(detail$Power.Col)
  
for (i in 1:4) {
  electuse <- read.xlsx(detail$File[i], 3, colIndex=1:ncols[i], endRow=30, header=T, stringsAsFactors=F, colClasses=c("character", rep("numeric", ncols[i]-1)))
  volume <- read.xlsx2(detail$File[i], 3, colIndex=1:ncols[i], startRow=detail$start[i], endRow=detail$end[i], header=F, stringsAsFactors=F, colClasses=c("character", rep("numeric", ncols[i]-1)))
  names(volume) <- names(electuse)
  
  # reconfigure power from wide to narrow format for processing
  kwh <- gather(electuse, Date, kwh, -DATE)
  kl  <- gather(volume, Date, vol, -DATE)
  kl$vol <- ifelse(is.nan(kl$vol),NA, kl$vol)

  # split DATE string into caisson number and power tariff
  # remove drainage pumps first
  kwh <- subset(kwh, DATE != "1O7" & DATE != "167" & DATE != "170")
  clist <- strsplit(kwh$DATE, ' ')
  caisson <- as.integer(rapply(clist, function(x) head(x,1)))
  tariff <- rapply(clist, function(x) tail(x,1))
  kwh <- data.frame(caisson, tariff, kwh[,2:3])
    
  # extract caisson from DATE
  names(kl)[1] <- 'caisson'

  # convert Date to character
  kwh$Date <- as.character(kwh$Date)
  kl$Date <-  as.character(kl$Date)

  # extract integer date from string and convert to dd/mm/YYYY
  # first kwh then kl
  kwh$Date <- substr(kwh$Date, 2, 6)
  kwh$Date <- as.Date(as.integer(kwh$Date), origin="1899-12-30", format="%Y-%m-%d")
  
  # kl
  kl$Date <- substr(kl$Date, 2, 6)
  kl$Date <- as.Date(as.integer(kl$Date), origin="1899-12-30", format="%Y-%m-%d")  
  
  # store hours in appropriate date dataframe
  if(i==1) use <- kwh else use <- rbind(use, kwh)
  if(i==1) Vol <- kl else Vol <- rbind(Vol, kl)
  }

Vol$caisson <- ifelse(Vol$caisson=="3" | Vol$caisson=="3_", "3a", Vol$caisson)
Vol$caisson <- ifelse(Vol$caisson=="4", "4a", Vol$caisson)
Vol$caisson <- ifelse(Vol$caisson=="5" | Vol$caisson=="5_", "5a", Vol$caisson)
Vol$caisson <- ifelse(Vol$caisson=="yandilla", "Ya", Vol$caisson)
Vol$caisson <- ifelse(Vol$caisson=="Out", "Oa", Vol$caisson)
  
str(use)
str(Vol)

# calculate electricity consumption
use$use[27:nrow(use)] <- use$kwh[27:nrow(use)] - use$kwh[1:(nrow(use)-26)]

# calculate volume pumped
# correct for conversion from kl to Ml
Vol$vol <- with(Vol, ifelse(caisson %in% c("4a","5a") & Date < "1999-10-01", vol/1000, vol))
Vol$vol <- with(Vol, ifelse(caisson=="4a" & (Date >="1999-10-01" & Date <= "2003-11-03") ,vol*1000, vol))

subset(Vol, caisson %in% c("4a","4A"))
subset(Vol, caisson %in% c("5a"))

# split caission to number and letter
caisson <- ifelse(nchar(Vol$caisson)==2, substr(Vol$caisson,1,1),' ')
subcaiss <- ifelse(nchar(Vol$caisson)==2, substr(Vol$caisson,2,2),Vol$caisson)
Vol <- data.frame(caisson, subcaiss, Vol[,2:3]) 

# test for negative values of power consumption
ivec <- which(use$use < 0)
use[ivec,]
if(length(ivec)>0) use$use[ivec] <- NA

# calculate volume pumped
Vol$Vol <- NA
Vol$Vol[6:310] <- Vol$vol[6:310] - Vol$vol[1:305]
Vol$Vol[324:nrow(Vol)] <- Vol$vol[324:nrow(Vol)] - Vol$vol[311:(nrow(Vol)-13)]

# test for negative values for Volume pumped
ivec <- which(Vol$Vol < 0)
Vol[ivec,]
if(length(ivec)>0) Vol$Vol[ivec] <- NA

# sort data on caisson name
use.srt <- use[order(use$caisson, use$tariff),]
head(use.srt, 20)
tail(use.srt, 20)

Vol.srt <- Vol[order(Vol$caisson, Vol$subcaiss),]
head(Vol.srt, 20)
tail(Vol.srt, 20)

# copy sorted data back to original
pumphrs <- pumphrs.srt
use <- use.srt
Vol <- Vol.srt
rm(pumphrs.srt, use.srt, Vol.srt)

# renumber sorted rows in sequence
rownames(pumphrs) <- 1:nrow(pumphrs)
rownames(use) <- 1:nrow(use)
rownames(Vol) <- 1:nrow(Vol)
Vol$caisson <- as.character(Vol$caisson)

# plot electricty consumption
qplot(Date, use, data=use, color=tariff, facets=~caisson, geom='line')

# plot vol pumped from caissons
qplot(Date, Vol, data=Vol, facets=~caisson, geom='line')
temp <- subset(Vol, !(caisson == "4" & subcaiss == "B"))
Vol <- temp
rm(temp, caiss04, caiss05)

# convert all caissons to ML
Vol$Vol <- with(Vol, ifelse(caisson %in% c("1","2","3","4","6"), Vol/1000, Vol))
