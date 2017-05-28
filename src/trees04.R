require(rpart)
require(DAAG)
require(partykit)
require(rpart.plot)
require(rattle)
require(compiler)

# define function to fine minimum sum of squares
  compile(nrows <- nrow(mallee)  
  for (i in 1:2500){
    rows <- sample(1:nrows, floor(0.8*nrows))
    train <- mallee[rows, c('taw','Field.CO3', 'Field.Texture')]
    test <-  mallee[-rows, c('taw','Field.CO3', 'Field.Texture')]
  
# derive regression tree
    taw.ct <- ctree(taw ~ Field.Texture + Field.CO3, data=train, control=ctree_control(teststat="quad", testtype="Bonferroni", mincriterion=0.97))
    test$pred <- predict(taw.ct, newdata=test)
    Taw.tab <- with(test, tapply(taw, list(Field.Texture, Field.CO3), mean, na.rm=T))
    pred.tab <- with(test, tapply(pred, list(Field.Texture, Field.CO3), mean, na.rm=T))
    Nobs= sum(!is.na(Taw.tab-pred.tab))
    sumsq <- sum((Taw.tab-pred.tab)^2, na.rm=T)
    sumsq <- sumsq/Nobs

  # on first iteration save regression tree information in min & max variables
    if (i==1) {
      minsumsq <- sumsq
      mintrain <- train
      mintest <- test
      mintaw.ct <- taw.ct
      min.i <- 1
      }
    
  # save regression tree information in min & max variables as appropriate
      else {
      if (sumsq < minsumsq) {
        minsumsq <- sumsq
        mintrain <- train
        mintest <- test
        mintaw.ct <- taw.ct
        min.i <- i
        }
      }
    }
  )

findMinTree(mallee)

findMinTree.bc <- compile(findMinTree)

# store the number of rows in the mallee dataframe
minss <- vector(mode="double", length=10)
seed <- vector(mode="integer", length=10)

# randomly sample 4 fifths of dataset without replacement
# generate a vector of 10 random seeds with the first seed as 592259
seed[1] <- 407885
set.seed(23)

for (n in 2:10) seed[n] <- floor(runif(1, min=1, max=999999))

for (n in 1:10) {
  set.seed(seed[n])

#  mallee$counts <- 0
  minss[n] <- eval(compile(findMinTree(mallee)))
  
  cat("Minimum sum of squares=", minss[n], " for seed[",n,"]=",seed[n], "\n")
  plot(mintaw.ct, main=paste("Minimum Mean Square Tree for seed[",n,"] = ",seed[n]))
}
