str(mrc)
head(mrc,20)
with(mrc, plot(Clay, raw2, pch=16, col="red"))
mrc_1 <- mrc[, c(6,17)]
head(mrc_1,20)
mrc_1 <- mrc_1[order(mrc_1$Sand),]
mrc_2 <- stepupbnd(mrc_1)
head(mrc_2,20)

with(mrc_1, plot(Sand, raw2, pch=16, col="blue"))
with(mrc_2, points(Sand, raw2, col="red"))

mrc.nls01 <- lm(I(1/raw2)~ns(Sand,5), data=mrc_2)
summary(mrc.nls01)
Sand <- seq(min(mrc_2$Sand), max(mrc_2$Sand),1)
newdata <- data.frame(Sand)
newdata$raw2 <- predict(mrc.nls01, newdata)
# head(newdata)
with(newdata, plot(Sand, 1/raw2, col="blue", type='l'))
with(mrc_2,  points(Sand, raw2, pch=16, col="red"))
with(mrc_1, points(Sand, raw2, pch=21))
