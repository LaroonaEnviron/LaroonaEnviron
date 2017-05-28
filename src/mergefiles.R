require(xlsx)

# read in major soil data file
(filein1 <- choose.files("*.xlsx"))
soils <- read.xlsx(filein1, 1, colClasses=c("integer","numeric","numeric","numeric","numeric","character","character","character","numeric","character","character","character","numeric","numeric","numeric","numeric","character"), colIndex=c(1:16,25), stringsAsFactors=FALSE)
str(soils)

# read in file with soil names
filein2 <- choose.files("*.xlsx")
soilnames <- read.xlsx(filein2, 1, colClasses=c("integer","character"), colIndex=1:2, stringsAsFactors=FALSE)
str(soilnames)

# add names from soilnames to soils
numVec <- match(soils$No, soilnames$No)
soils$name <- soilnames[numVec, 2]
str(soils)

# correct 'Loxton I A' to 'Loxton IA'
soils$Location <- with(soils, ifelse(Location=="LOXTON I A", "LOXTON IA", soils$Location)) 

# convert horizon to upper, lower depths
hLst <- strsplit(soils$Horizon, '[-]')
depths <- unlist(hLst)
lVec <- (1:length(depths)) %% 2
upper <- integer()
lower <- integer()
for (i in 1:length(depths)) {
  if(lVec[i]) upper[i] <- depths[i] else lower[i] <- depths[i]
  }

# remove NA values
upper <- upper[!is.na(upper)]
lower <- lower[!is.na(lower)]
soils$upper <- as.integer(upper)
soils$lower <- as.integer(lower)
str(soils)
head(soils[, c(1:2, 18:20)],50)

# output files to a 'csv' file
write.xlsx(soils, , file='../data/soilsMerge.xlsx', sheetName="merged")
