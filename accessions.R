# estimate volume pumped
require(xlsx)

# read in Excel sheet
filein <- choose.files("*.xlsx")
cobby <- read.xlsx2(filein, sheetName='Cobdogla', stringsAsFactors=F, header=T, colClasses=c('integer',rep('numeric',16)))
str(cobby)
head(cobby, 20)
cobby <- cobby[7:121,]

$ calculate pumped vol with variation
set.seed(12347)
rate <- with(cobby, ifelse (year <= 1931, 11, 
        ifelse(year > 1931 & year <= 1970, 10.5, 9.5)))

colour <- with(cobby, ifelse (year <= 1931, 'green', 
        ifelse(year > 1931 & year <= 1970, 'blue', 'cyan')))
        
cobby$pump2 <- with(cobby, ifelse(year < 1992, area*rnorm(nrow(cobby),rate,0.5), pumped))
cobby[1:50, c(1,2,3,18)]
with(cobby, plot(pumped,pump2, type='p', xlab='Original', ylab='Random'))
with(cobby, {
  plot(year, pumped, col=colour, type='p', pch=16, xlab='Original', ylab='Random')
  points(year, pump2)
  })
