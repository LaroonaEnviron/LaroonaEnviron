require(randomForest)
require(ROCR)
require(rpart)
require(partykit)

# divide dataset into  a training & validation datasets
# set random seed so get repeatedable results
# sample with replacement. Training set as 2/3 of total number of rows in dataset.
# Validation dataset is rows not selected in training dataset
# select training dataset. Remember to run set.seed each time
set.seed(7001)
sampleNos <- sample(1:nrow(mallee), ceiling(nrow(mallee)*2/3), replace=TRUE)
train.ds <- mallee[sort(sampleNos),]
valid.ds <- mallee[-unique(sampleNos),]
head(train.ds[, 1:6], 50)
head(valid.ds[, 1:6], 50)
str(train.ds)

#generate random forests 
mallee.rf <-randomForest(raw2~upper+lower+Field.CO3+Field.Texture, data=train.ds, ntree=10000, prox=T, keep.forest=T)

MDSplot(mallee.rf, mallee$Field.CO3, k=4)

data("WeatherPlay", package = "partykit")
WeatherPlay

sp_o <- partysplit(1L, index = 1:3)
sp_h <- partysplit(3L, breaks = 75)
sp_w <- partysplit(4L, index = 1:2)

pn <- partynode(1L, split = sp_o, kids = list(
      partynode(2L, split = sp_h, kids = list(
      partynode(3L, info = "yes"),
      partynode(4L, info = "no"))),
      partynode(5L, info = "yes"),
      partynode(6L, split = sp_w, kids = list(
      partynode(7L, info = "yes"),
      partynode(8L, info = "no")))))

pn
py <- party(pn, WeatherPlay)
print(py)

plot(py)

predict(py, head(WeatherPlay))
