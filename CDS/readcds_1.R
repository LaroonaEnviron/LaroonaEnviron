# read in cds data
require(xlsx)
require(tidyr)

# read in details of each CDS spreadsheet
detfile <- "RIT_CDS_details.xlsx" 
detail <- read.xlsx2(detfile, 1, header=T, stringsAsFactors=F, colClasses=c(rep("character",3), rep('numeric',2)))

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
pumphrs$time[27:nrow(pumphrs)] <- with(pumphrs, hrs[27:nrow(pumphrs)]-hrs[1:(nrow(pumphrs)-26)])

# correct negative hours
ivec <- with(pumphrs, which(time < -1000))
pumphrs[ivec,]

pumphrs$time[ivec] <- pumphrs$hrs[ivec]
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
  
  # split DATE string into caisson number and power tariff
  # remove drainage pumps first
  kwh <- subset(kwh, DATE != "1O7" & DATE != "167" & DATE != "170")
  clist <- strsplit(kwh$DATE, ' ')
  caisson <- as.integer(rapply(clist, function(x) head(x,1)))
  tariff <- rapply(clist, function(x) tail(x,1))
  kwh <- data.frame(caisson, tariff, kwh[,2:3])
    
  # extract caisson from DATE
  clist <- strsplit(kl$DATE, ' ')
  browser()
  caisson <- as.integer(rapply(clist, function(x) head(x,1)))

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

str(use)
str(Vol)

# calculate electricity consumption
use$use[27:nrow(use)] <- use$kwh[27:nrow(use)]-use$kwh[1:(nrow(use)-26)]

# sort data on caisson name
use.srt <- use[order(use$caisson, use$tariff),]
head(use.srt, 200)
tail(use.srt, 200)

# copy sorted data back to original
pumphrs <- pumphrs.srt
use <- use.srt
rm(pumphrs.srt, use.srt)

# renumber sorted rows in sequence
rownames(pumphrs) <- 1:nrow(pumphrs)
rownames(use) <- 1:nrow(use)

# correct negative use 
ivec <- with(use, which(use < 0))
use[ivec, ]
use$use[ivec] <- use$kwh[ivec]

# plot electricty consumption
qplot(Date, use, data=use, color=tariff, facets=~caisson, geom='line')
