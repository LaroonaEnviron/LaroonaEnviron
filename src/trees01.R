require(partykit)
require(ROCR)
require(rpart)
require(DAAG)

str(mallee)
with(mallee_1, plot(Texture, raw2, pch=15+CO3, col=CO3, xlab="Texture Class", ylab="Readily Available Water (mm/m)"))

with(mallee, bwplot(raw2~Field.Texture | Field.CO3, xlab="Texture Class", ylab="Readily Available Water (mm/m)"))

with(mallee, table(Field.Texture, Field.CO3, dnn=list('Field Texture','Carbonate Reaction')))
averaw <- with(mallee, tapply(raw2, list(Field.Texture,Field.CO3), mean, na.rm=TRUE))
round(averaw, 0)

range(mallee$raw2)
mallee$rawcat <- with(mallee, ifelse(raw2 < 40, "30s", 
     ifelse(raw2 >=40 & raw2 < 50, "40s", 
     ifelse(raw2 >=50 & raw2 < 60, "50s",
     ifelse(raw2 >=60 & raw2 < 80,"60-80", ">80")))))

with(mallee, tapply(raw2, list(Field.Texture, Field.CO3, rawcat), mean, na.rm=TRUE))

raw.ctree <- ctree(raw2~upper+lower+Field.Texture+Field.CO3, data=mallee)
str(raw.ctree)
summary(raw.ctree)
plot(raw.ctree)

raw.rp <- rpart(raw2~upper+lower+Field.Texture+Field.CO3, data=mallee, method="anova")
plot(raw.rp)
text(raw.rp)

raw.ctree$node[1]
