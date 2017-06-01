#require(xlsx)
#require(RColorBrewer)
blues <- colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE) #colorRampPalette(c("lightblue","navyblue"))
browns <- brewer.pal(9, "YlOrBr")[c(1,2,4,6,7)]

# read in major soil data file
#(filein1 <- choose.files("*.xlsx"))
filein1 <- "D:\\Laroona Consulting\\MRC\\data\\soilsMerge.xlsx"
soils <- read.xlsx(filein1, 1, colClasses=c("integer","numeric","numeric","numeric","numeric","integer","integer","numeric","numeric","numeric","numeric","numeric","character","character","character","character","character","character", "character"), colIndex=c(1:20), stringsAsFactors=FALSE)
#str(soils)

# convert State, Location, Texture, Field.Texture, Field.CO3 & Site from character to factor in dataframe soils
soils$State <- factor(soils$State)
soils$Location <- factor(soils$Location)

# convert Texture, Field.Texture & Field.CO3 to ordered factors
soils$Texture <- factor(soils$Texture, levels=c("SAND","LS","SL","LOAM","SCL","CL","CLAY"), ordered=TRUE)
soils$Field.Texture <- factor(soils$Field.Texture, levels=c("S","LS","SL","L","LSCL","SCL","CL","C"), ordered=TRUE)
soils$Field.CO3 <- factor(soils$Field.CO3, levels=,c("N","S","M","H","V"), ordered=TRUE)

soils$Region <- factor(soils$Region)
#str(soils)

#head(soils, 50)

# Use soil names as row names
rownames(soils) <- soils[,20]

# remove No and name
soils <- soils[, -c(1, 20)]

# calculate total available water (totaw) and readily available water rdyaw
soils$totaw <- with(soils, (KPa8 - KPa1500)*1000)
soils$raw60 <- with(soils, (KPa8 - KPa60)*1000)

# subset mallee only soils
mallee <- subset(soils, Region=="Riverland" | Region=="Sunraysia")
mallee$Region <- factor(mallee$Region)
mallee$Field.Texture <- factor(mallee$Field.Texture)
