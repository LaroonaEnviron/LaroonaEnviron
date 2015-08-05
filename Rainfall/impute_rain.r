require(xlsx)
require(Amelia)
require(DAAG)

# read in amended Berri rainfall
xlsxin <- choose.files("*.xlsx")
berri <- read.xlsx2(xlsxin, sheetName="Berri", stringsAsFactors=F, header=T, colClasses=c(rep('character',2), rep('numeric', 14)))
str(berri)

# replace NaN with NA
for (i in 4:16) {
  berri[,i] <- ifelse(is.nan(berri[, i]), NA, berri[,i])
}

head(berri, 20)

# impute missing values
random.imp <- function (a){
missing <- is.na(a)
n.missing <- sum(missing)
a.obs <- a[!missing]
imputed <- a
imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
return (imputed)
}

N <- nrow(berri)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
X <- data.frame(rep(berri[,3], times=2), rep(c("obs","imp"), each=N))
names(X) <- c("year","values")
for (m in 1:12) {
  X[,m+2] <- c(berri[,m+3], random.imp(berri[,m+3]))
  names(X)[m+2] <- months[m]
  print(histogram( ~ X[,m+2] | X$values, xlab="Rainfall (mm)", main=paste("Histogram of rainfall for the month of ",months[m])))
  pause()
  }
X$Annual <- rowSums(X[, 3:14])
berri[1:N, 4:16] <- X[(N+1):(2*N), 3:15]
head(berri, 20)
write.xlsx(berri, xlsxin, sheetName="Berri_Imputed", showNA=TRUE, append=TRUE)

months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#_________________________ using amelia algorithm __________________________________
berri.imp <- amelia(berri[,1:15], m=20, p2s=1, idvars=c("Product.code","Station.Number"), ts="Year", splinetime=3)
par(mfrow=c(2,2))
plot(berri.imp, which.vars=c(4:15))
par(mfrow=c(1,1))
disperse(berri.imp, m=20, dims=1)

nrow <- nrow(berri.imp$imp[[1]])
ncol <- berri.imp$m
ave_mm <- matrix(nrow=nrow, ncol=ncol)
for (i in 1:berri.imp$m) ave_mm[,i] <- berri.imp$imp[[i]]$Jan
mm <- rowMeans(ave_mm)
