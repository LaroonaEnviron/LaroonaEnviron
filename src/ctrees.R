require(partykit)
require(ROCR)
require(rpart)
require(partykit)
require(DAAG)
str(mallee)

set.seed(7)
nlmnts <- nrow(mallee)
# randomly sample 80% of dataset without replacement
for (i in 1:10) {
  rows <- sample(1:nlmnts, floor(nlmnts*0.8), replace=FALSE)
  sort(rows)
  mallee.train <- mallee[rows, ]
  mallee.test <- mallee[-rows,]

  # construct conditional inference tree
  raw.ctree <- ctree(raw2~upper+lower+Field.Texture+Field.CO3, data=mallee.train)
  plot(raw.ctree)
  pause()
}
