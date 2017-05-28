require(rpart)
require(DAAG)

# grow a tree
# set random seed
nlmnts <- nrow(mallee)

require(partykit)
require(DAAG)
str(mallee)
set.seed(7)
nlmnts <- nrow(mallee)
for (i in 1:10) {
  # randomomly sample a third of dataset without replacement
  rows <- sample(1:nlmnts, floor(nlmnts*2/3), replace=T)
  mallee.sub <- mallee[rows, ]

  # construct conditional inference tree
  raw.ctree <- ctree(raw2~upper+lower+Field.Texture+Field.CO3, data=mallee.sub)
  raw.ctree
  plot(raw.ctree)
  pause()  
  }

# Random Forest prediction of Kyphosis data
require(randomForest)
fit <- randomForest(raw2 ~ upper+lower+Field.Texture+Field.CO3, data=mallee, importance=T, ntree=100)
print(fit)
plot(fit) # view results
varImpPlot(fit, pch=16, col="blue")

# gradient boosted model
require(gbm)
raw.gbm <- gbm(raw2 ~ upper+lower+Field.Texture+Field.CO3, data=mallee, distribution='gaussian', n.trees=1500, n.cores=NULL, train=0.6)
raw.gbm
plot(raw.gbm, i.var=c(1,4), pch=16, col='blue')
summary(raw.gbm)
pretty.gbm.tree(raw.gbm)
str(raw.gbm)

require(dismo)
raw.dis <- gbm.step(mallee, gbm.x=c("upper","lower","Field.Texture","Field.CO3"), gbm.y=c(20), family="gaussian", tree.complexity=4)
gbm.plot(raw.dis, smooth=TRUE, plot.layout=c(1,4))
summary(raw.dis)

# linear regression
require(car)
require(MASS)
options(contrasts=c("contr.treatment","contr.helmert"))
summary(powerTransform(raw2~Field.Texture+Field.CO3+upper+lower, data=mallee))

set.seed(7)
nlmnts <- nrow(mallee)
for (i in 1:10) {
  # randomomly sample a third of dataset without replacement
  rows <- sample(1:nlmnts, floor(nlmnts*2/3), replace=T)
  mallee.sub <- mallee[rows, ]

  # obtain linear model
  raw.lm <- lm(sqrt(raw2)~Field.Texture+Field.CO3+upper+lower, data=mallee.sub)
  print(anova(raw.lm))
  print(summary(raw.lm))
  par(mfrow=c(2,2))
  plot(raw.lm)
  par(mfrow=c(1,1))
  pause()  
  }

