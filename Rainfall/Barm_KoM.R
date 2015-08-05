require(xlsx)
require(Amelia)
require(DAAG)

# read in amended Barmera & Kingston-on-Murray rainfall
xlsxin <- choose.files("*.xlsx")
barmera <- read.xlsx2(xlsxin, sheetName="Barmera", stringsAsFactors=F, header=T, colClasses=c(rep('character',2), rep('numeric', 14)))
str(barmera)

kom <- read.xlsx2(xlsxin, sheetName="Kingston-on-Murray", stringsAsFactors=F, header=T, colClasses=c(rep('character',2), rep('numeric', 14)))
str(kom)

# replace NaN with NA
for (i in 4:16) {
  barmera[,i] <- ifelse(is.nan(barmera[, i]), NA, barmera[,i])
  kom[,i] <- ifelse(is.nan(kom[, i]), NA, kom[,i])
}

head(barmera, 20); head(kom, 20)

cobdogla <- kom
cobdogla[,2] <- paste(barmera[,2],'_3', sep='')

for (year in 1:132) {
  for(mnth in 4:15) {
    cobdogla[year, mnth] <- ifelse(is.na(kom[year,mnth]) & is.na(barmera[year,mnth]), NA, 
    round(mean(c(kom[year,mnth],barmera[year,mnth]),na.rm=T),1))
    }
  }
head(cobdogla, 20)

N <- nrow(cobdogla)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
X <- data.frame(rep(cobdogla[,3], times=2), rep(c("obs","imp"), each=N))
names(X) <- c("year","values")
for (m in 1:12) {
  X[,m+2] <- c(cobdogla[,m+3], random.imp(cobdogla[,m+3]))
  names(X)[m+2] <- months[m]
  print(histogram( ~ X[,m+2] | X$values, xlab="Rainfall (mm)", main=paste("Histogram of rainfall for the month of ",months[m])))
  pause()
  }
  
X$Annual <- round(rowSums(X[, 3:14]),1)
cobdogla[1:N, 4:16] <- X[(N+1):(2*N), 3:15]
head(cobdogla, 20)
write.xlsx(cobdogla, xlsxin, sheetName="Cobby_Imputed", showNA=TRUE, append=TRUE)
