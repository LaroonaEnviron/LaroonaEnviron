mrc1 <- mrc[order(mrc$Sand),]
out <- with(mrc1, boxplot(raw1~Field_CO3))
idx <- match(out$out, mrc1$raw1)
with(mrc1, plot(raw1~Sand, pch=21, col="blue"))
mrc2 <- mrc1[-idx,]
with(mrc2, points(raw1~Sand, pch=16, col="red"))

bvals <- boundaryvalue(mrc2, 6, 17)
with(mrc2, plot(Sand, raw2, pch=21, col="blue"))
with(bvals, points(Sand, raw2, pch=16, col="red"))

raw.ns01 <- lm(raw2~ns(Sand, 4), data=bvals)
summary(raw.ns01)
par(mfrow=c(2,2))
plot(raw.ns01)
par(mfrow=c(1,1))

Sand <- seq(min(bvals$Sand), max(bvals$Sand),1)
newdata <- data.frame(Sand)
newdata$raw2 <- predict(raw.ns01, newdata)
with(bvals, plot(Sand, raw2, pch=16, col="red"))
with(newdata, lines(Sand, raw2, lty=2, col="darkgreen"))
with(mrc2, points(Sand, raw2, pch=21, col="blue"))

raw.ns02 <- lm(raw2~ns(Sand, 5), data=bvals)
anova(raw.ns02, raw.ns01)
summary(raw.ns02)
par(mfrow=c(2,2))
plot(raw.ns02)
par(mfrow=c(1,1))

newdata$raw2 <- predict(raw.ns02, newdata)
with(bvals, plot(Sand, raw2, pch=16, col="red"))
with(newdata, lines(Sand, raw2, lty=2, col="darkgreen"))
with(mrc2, points(Sand, raw2, pch=21, col="blue"))

# total available water vs Clay
mrc1 <- mrc[order(mrc$Clay),]
bxout <- with(mrc1, boxplot(taw~Field_CO3))
idx <- match(bxout$out, mrc1$taw)
with(mrc1, plot(taw~Clay, pch=16, col="blue"))
mrc2 <- mrc1[-idx,]
with(mrc2, points(taw~Clay, pch=21, col="red"))

bvals <- boundaryvalue(mrc2, 5, 18)
with(mrc2, plot(Clay, taw, pch=21, col="blue"))
with(bvals, points(Clay, taw, pch=16, col="red"))

taw.ns01 <- lm(taw~ns(Clay, 4), data=bvals)
summary(taw.ns01)
par(mfrow=c(2,2))
plot(taw.ns01)
par(mfrow=c(1,1))

Clay <- seq(min(bvals$Clay), max(bvals$Clay),1)
newdata <- data.frame(Clay)
newdata$taw <- predict(taw.ns01, newdata)
with(bvals, plot(Clay, taw, pch=16, col="red"))
with(newdata, lines(Clay, taw, lty=2, col="darkgreen"))
with(mrc2, points(Clay, taw, pch=21, col="blue"))
