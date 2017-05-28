# Generate data
library(reshape2) # for melt
str(volcano)
class(volcano)
head(volcano, 20)
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
str(volcano3d)
head(volcano3d)

# Basic plot
v <- ggplot(volcano3d, aes(x, y, z = z))
v + stat_contour()

filein <- choose.files("*.dat")
mallee.grid <- read.csv(filein, header=T)
str(mallee.grid)
head(mallee.grid, 25)
mallee.grid$RAW <- with(mallee.grid, ifelse(RAW > 1.0e+38, NA, RAW))
head(mallee.grid, 25)

v <- ggplot(mallee.grid, aes(Clay, Carbonate, z = RAW))
v + stat_contour(geom="polygon", aes(fill=..level..))

require(rgl)
with(mallee.grid, persp3d(Carbonate, Clay, RAW))

require(rgdal)
gridfile <- choose.files("*.grd")
GDALinfo(gridfile)
Grid <- readGDAL(gridfile)
str(Grid)
head(Grid)
plot(Grid)