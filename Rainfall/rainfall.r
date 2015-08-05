require(xlsx)

# get Renmark spreadsheet filename and read in
xlsx.ren <- choose.files("*.xlsx")
renmark <- read.xlsx(xlsx.ren, 1)

# get Lyrup spreadsheet filename and read in
xlsx.lyr <- choose.files("*.xlsx")
lyrup <- read.xlsx(xlsx.lyr, 1)
head(renmark, 20); head(lyrup, 20)
str(renmark); str(lyrup)

# create combined spreadshhet from renmark and lyrup by taking the monthly mean
combined <- renmark[,3:15]
for (i in 1:nrow(renmark)) 
  for(j in 4:15) {
    combined[i,(j-2)] <- ifelse(!is.na(renmark[i,j]) & !is.na(lyrup[i,j]), mean(c(renmark[i,j], lyrup[i,j])),                          mean(c(renmark[i,j], lyrup[i,j]), na.rm=T))
  }

head(combined, 20)

# write out the combined mean result and the individual spreadsheets
xlsxout <-  "C:\\Users\\TonyM\\Laroona Consulting\\IIM\\PikeMurtho\\rainfall\\rainfall.xlsx"
write.xlsx(renmark, xlsxout, sheetName="renmark", showNA=TRUE, append=FALSE)
write.xlsx(lyrup, xlsxout, sheetName="lyrup", showNA=TRUE, append=TRUE)
write.xlsx(combined, xlsxout, sheetName="combined", showNA=TRUE, append=TRUE)

# read combined sheet back in
xlsxin <- xlsxout
combined <- read.xlsx(xlsxin, 3)
head(combined, 20)

# impute missing values
random.imp <- function (a){
missing <- is.na(a)
n.missing <- sum(missing)
a.obs <- a[!missing]
imputed <- a
imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
return (imputed)
}

require(DAAG)
N <- nrow(combined)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
X <- data.frame(rep(combined[,1], times=2), rep(c("obs","imp"), each=N))
names(X) <- c("year","values")
for (m in 1:12) {
  X[,m+2] <- c(combined[,m+1], random.imp(combined[,m+1]))
  names(X)[m+2] <- months[m]
  print(histogram( ~ X[,m+2] | X$values, xlab="Rainfall (mm)", main=paste("Histogram of rainfall for the month of ",months[m])))
  pause()
  }

combined[1:N, 2:13] <- X[(N+1):(2*N), 3:14]
head(combined, 20)
write.xlsx(combined, xlsxout, sheetName="imputed", showNA=TRUE, append=TRUE)
 