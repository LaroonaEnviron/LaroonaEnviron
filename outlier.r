outlier <- function(S0, xpos, ypos, npoints) {
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#| outlier identifies data points that are seperated from the remaining data  |
#| by a distance determined in terms of both x and y variables of interest.   |
#| Algorithm is an impementation of the method described in E Schnug, J Heym, |
#| and F Achwan (1996) Comm. Soil Sci and Plant Anal. 27 pp2739-2748          |
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #browser()
  #  sort in ascending order  and copy to new dataframe
  S1 <- S0[,c(xpos, ypos)]
  S1 <- S1[order(S1[,1]),]

  # calculate standard deviation
  sd.x <- sqrt(var(S1[,1], na.rm=T)/(nrow(S1)-sum(is.na(S1[,1]))))
  sd.y <- sqrt(var(S1[,2], na.rm=T)/(nrow(S1)-sum(is.na(S1[,2]))))

  # construct rectangle and test number of points in rectangle
  idx <- NULL
  for (i in 1:nrow(S1)) {
  xi <- S1[i,1]
  yi <- S1[i,2]
  temp <- subset(S1, (S1[,1] >= xi-sd.x & S1[,1] <= xi+sd.x) &
                     (S1[,2] >= yi-sd.y & S1[,2] <= yi+sd.y))
   if (nrow(temp) >= npoints) idx[i] <- T else idx[i] <- F
   }
   # browser()
   S1 <- S1[(idx==T),]
   cat("No of outliers =", sum(idx==F), "\n")

  return (S1)
  }

mrc_1 <- outlier(mrc, 5,9, 0.5, 0.5, 4)
with(mrc, plot(Clay, Lab_CO3, pch=16))
with(mrc_1, points(Clay,Lab_CO3, pch=16, col=2))
with(mrc_1, plot(Clay, Lab_CO3, pch=16))

stepupbnd <- function(S1) {
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#| stepupbnd finds the upper boundary points of a dataset S1 so that a bound- |
#| ary line can be fitted.  Algorithm is an implementation of the method      |
#| described in E Schnug, J Heym, and F Achwan (1996) Comm. Soil Sci and Plant|
#| Anal. 27 pp2739-2748                                                       |
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# S1 is a dataframe of two variables
  # find maximim y value

  max.y <- max(S1[,2], na.rm=T)
  myp <- which(S1[,2]==max.y)

  # cat("standard error of mean= ", se, "\n")

  # Construct Boundary points
  S2 <- S1
  for (i in 1:(myp-1))
    if (S1[i+1,2] < S2[i,2]) S2[i+1,2] <- S2[i,2] else S2[i+1,2] <- S1[i+1,2]

  for (i in nrow(S1):(myp+2))
    if (S1[i-1,2] < S2[i,2]) S2[i-1,2] <- S2[i,2] else S2[i-1,2] <- S1[i-1,2]
    
  S2[nrow(S1),2] <- S1[nrow(S1),2]

  return (S2)
  }


mrc_2 <- stepupbnd(mrc)

with(mrc_2, plot(Lab_CO3~Clay, pch=16, col="blue"))
with(mrc, points(Lab_CO3~Clay, pch=17, col="red"))

notdup <- which(!duplicated(mrc_2$Lab_CO3))
which(unique(mrc_2$Lab_CO3))
with(mrc_2[notdup,], plot(Lab_CO3~Clay, pch=16, col="blue"))
with(mrc_2, points(Lab_CO3~Clay, pch="+", col="red"))

mrc.nls01 <- nls(Lab_CO3~(a*Clay^2)/(b+c*Clay+Clay^2), data=mrc_2, start=list(a=0.2,b=1/17,c=1))
summary(mrc.nls01)
with(mrc_1, plot(Lab_CO3~Clay, pch=16, col="blue"))
lines(mrc_2$Clay, fitted(mrc.nls01))
lines(mrc_2[notdup,]$Clay, fitted(mrc.nls02), col=2, lty=2)

mrc.nls02 <- nls(Lab_CO3~ (a*Clay^2)/(b+c*Clay+Clay^2), data=mrc_2[notdup,], start=list(a=0.1,b=174,c=-21))
summary(mrc.nls02)
with(mrc_2[notdup,], plot(Lab_CO3~Clay, pch=16, col="blue"))
lines(mrc_2[notdup,]$Clay, fitted(mrc.nls02))
