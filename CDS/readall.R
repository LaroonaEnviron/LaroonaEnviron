# read in cds data
require(xlsx)
require(tidyr)

# read in details of each CDS spreadsheet
detfile <- "RIT_CDS_details.xlsx" 
detail <- read.xlsx2(detfile, 1, header=T, stringsAsFactors=F, colClasses=rep("character",3))

ncols <- position(detail$Power.Col)

sprdsht <- read.xlsx2(detail$File[4], 3, colIndex=1:ncols[4], header=T, stringsAsFactors=F)
str(sprdsht)

# extract meter readings and flow data
meter <- sprdsht[42:52,]
flow <- sprdsht[97:107,]
names(meter)[1] <- "caisson"
names(flow)[1] <- "caisson"
str(meter)
str(flow)

nch <- nchar(meter$caisson)
strList <- strsplit(meter$caisson, ' ')

for (n in 1:length(nch)) {
   if (n %in% c(5,9)) p <- 5 
   else { if (n==8) p <- 6
     else p <- 3}
   meter$caisson[n] <- strList[[n]][p]
  } 
meter[, 1:4]

nch <- nchar(flow$caisson)
strList <- strsplit(flow$caisson, ' ')

for (n in 1:length(nch)) {
   if (n %in% c(5,9)) p <- 5 
   else { if (n==8) p <- 7
     else p <- 3}
   flow$caisson[n] <- strList[[n]][p]
  } 
flow[, 1:4]

str(flow)
drain <- gather(flow, Date, vol, -caisson)
mdrain <- gather(meter, Date, rdng, -caisson)  

# extract integer date from string and convert to dd/mm/YYYY
mdrain$Date <- substr(mdrain$Date, 2, 6)
mdrain$Date <- as.Date(as.integer(mdrain$Date), origin="1899-12-30", format="%Y-%m-%d")
mdrain$rdng <- as.numeric(mdrain$rdng)
head(mdrain,20)

drain$Date <- substr(drain$Date, 2, 6)
drain$Date <- as.Date(as.integer(drain$Date), origin="1899-12-30", format="%Y-%m-%d")
drain$vol <- as.numeric(drain$vol)
str(drain)

drain$vol[12:nrow(drain)] <- mdrain$rdng[12:nrow(drain)] - mdrain$rdng[1:(nrow(drain)-11)]

head(drain, 50)
negindx <- which(drain$vol < 0)
drain[negindx,]
mdrain[c(9,20, 42,53, 173,184),]

qplot(Date, vol, data=drain, facets=~caisson, geom="line")
   