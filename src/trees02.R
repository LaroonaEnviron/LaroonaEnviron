require(rpart)
require(DAAG)
require(partykit)
require(rpart.plot)
require(rattle)

# store the number of rows in the mallee dataframe
nrows <- nrow(mallee)

set.seed(11)

# randomly sample 4 fifths of dataset without replacement
for (i in 1:1000){
  rows <- sample(1:nrows, floor(0.8*nrows))
  train <- mallee[rows, c('raw2','Field.CO3', 'Field.Texture')]
  test <-  mallee[-rows, c('raw2','Field.CO3', 'Field.Texture')]

# derive regression tree
  raw.rp <- rpart(raw2 ~ Field.Texture + Field.CO3, data=train, method='anova')
  test$pred <- predict(raw.rp, newdata=test)
  raw2.tab <- with(test, tapply(raw2, list(Field.Texture, Field.CO3), mean, na.rm=T))
  pred.tab <- with(test, tapply(pred, list(Field.Texture, Field.CO3), mean, na.rm=T))
  Nobs= sum(!is.na(raw2.tab-pred.tab))
  sumsq <- sum((raw2.tab-pred.tab)^2, na.rm=T)
  sumsq <- sumsq/Nobs

  # on first iteration save regression tree information in min & max variables
  if (i==1) {
    minsumsq <- sumsq
    maxsumsq <- minsumsq
    mintrain <- train
    maxtrain <- train
    mintest <- test
    maxtest <- test
    minraw.rp <- raw.rp
    maxraw.rp <- raw.rp
    }
    
  # save regression tree information in min & max variables as appropriate
    else {
    if (sumsq < minsumsq) {
      minsumsq <- sumsq
      mintrain <- train
      mintest <- test
      minraw.rp <- raw.rp
      }
    if (sumsq > maxsumsq) {
      maxsumsq <- sumsq
      maxtrain <- train
      maxtest <- test
      maxraw.rp <- raw.rp
      }
    }
  }

# print out min & max mean squares
minsumsq; maxsumsq

par(mfrow=c(1,2))
plot(minraw.rp, main="Minimum Mean Square Tree")
text(minraw.rp)
plot(maxraw.rp, main="Maximum Mean Square Tree")
text(maxraw.rp)
par(mfrow=c(1,1))

# fancier plotting
prp(minraw.rp)
fancyRpartPlot(minraw.rp)

# test using conditional partioning
set.seed(632087)

# randomly sample 4 fifths of dataset without replacement
for (i in 1:2500){
  rows <- sample(1:nrows, floor(0.80*nrows))
  train <- mallee[rows, c('raw2','Field.CO3', 'Field.Texture')]
  test <-  mallee[-rows, c('raw2','Field.CO3', 'Field.Texture')]

# derive regression tree
  raw.ct <- ctree(raw2 ~ Field.Texture + Field.CO3, data=train, control=ctree_control(teststat="quad", testtype="Bonferroni", mincriterion=0.97))
  test$pred <- predict(raw.ct, newdata=test)
  Raw.tab <- with(test, tapply(raw2, list(Field.Texture, Field.CO3), mean, na.rm=T))
  pred.tab <- with(test, tapply(pred, list(Field.Texture, Field.CO3), mean, na.rm=T))
  Nobs= sum(!is.na(Raw.tab-pred.tab))
  sumsq <- sum((Raw.tab-pred.tab)^2, na.rm=T)
  sumsq <- sumsq/Nobs

  # on first iteration save regression tree information in min & max variables
  if (i==1) {
    minsumsq <- sumsq
    mintrain <- train
    mintest <- test
    minraw.ct <- raw.ct
    min.i <- 1
    }
    
  # save regression tree information in min & max variables as appropriate
    else {
    if (sumsq < minsumsq) {
      minsumsq <- sumsq
      mintrain <- train
      mintest <- test
      minraw.ct <- raw.ct
      min.i <- i
      }
    }
  }

# print out minimum mean squares
cat("Minimum mean square", minsumsq, " at iteration", min.i, "\n")

# plot of minimum conditional partitioned tree 
plot(minraw.ct, main="Minimum Mean Square Tree")

# save terminal nodes names to dataframe
nodes <- predict(minraw.ct, newdata=mallee, type="node")
mallee$rawclass <- ifelse(nodes==3, "Node03", 
  ifelse(nodes==4, "Node04", 
    ifelse(nodes==6, "Node06",
      ifelse(nodes==8, "Node08","Node09"))))
mallee$rawclass <- factor(mallee$rawclass)
str(mallee)

# extract and print mean Readily Availabe Water for each terminal node
head(mallee[, c(10,11,19,20,22)], 10)
with(mallee, tapply(raw2, rawclass, mean, na.rm=T))

# Are each node mean different
# contrasts are treatment contrasts
contrasts(mallee$rawclass) <- cbind(c(0,1,0,0,0), c(0,0,1,0,0), c(0,0,0,1,0), c(0,0,0,0,1))
options(contrasts=c("contr.treatment","contr.helmert"))
contrasts(mallee$rawclass)
lm01 <- lm(log(raw2)~rawclass, data=mallee)
anova(lm01)
summary(lm01)

raw_sp1 <- partysplit(2L, breaks=3)
raw_sp2 <- partysplit(3L, breaks = 1)
raw_sp3 <- partysplit(3L, breaks = 5)
raw_sp4 <- partysplit(3L, breaks = 6)

raw_node <- partynode(1L, split=raw_sp1, kids=list(
  partynode(2L, split=raw_sp2, kids=list(
    partynode(3L, info="39mm"),
    partynode(4L, info="59mm"))),
  partynode(5L, split=raw_sp3, kids=list(
    partynode(6L, info="78mm"),
    partynode(7L, split=raw_sp4, kids=list(
      partynode(8L, info="67mm"),
      partynode(9L, info="56mm")))))))
# extract subset of mallee dataframe, rename variables and rename Reaction levels
# then refactor
mallee.r <- mallee[, c('raw2','Field.CO3', 'Field.Texture')] 
names(mallee.r) <- c('Moisture','Reaction','Texture')
mallee.r$Reaction <- with(mallee.r, ifelse(Reaction=='N','Nil', 
  ifelse(Reaction=='S', 'Slight',
    ifelse(Reaction=='M', 'Medium',
      ifelse(Reaction=='H', 'High','Very High')))))
mallee.r$Reaction <- factor(mallee.r$Reaction, levels=c('Nil','Slight','Medium','High','Very High'), ordered=TRUE)

# rename Texture and refactor
mallee.r$Texture <- with(mallee.r, ifelse(Texture=='S','Sand', 
  ifelse(Texture=='LS', 'Loamy Sand',
    ifelse(Texture=='SL', 'Sandy Loam',
      ifelse(Texture=='LSCL', 'Light Sandy Clay Loam',
        ifelse(Texture=="SCL", 'Sandy Clay Loam',
          ifelse(Texture=='CL', 'Clay Loam', 'Clay')))))))
mallee.r$Texture <- factor(mallee.r$Texture, levels=c('Sand','Loamy Sandy','Sandy Loam','Light Sandy Clay Loam','Sandy\n Clay Loam','Clay Loam','Clay'), ordered=TRUE)
str(mallee.r) 
          
raw_pty <- party(raw_node, mallee.r)
print(raw_pty, terminal_panel=function(n) c(", Readily Available Water is ", n$info))
plot(raw_pty, terminal_panel=node_terminal(raw_pty, fill="light grey"), inner_panel=node_inner(raw_pty, fill="grey"))

mallee.r$rawclass <- predict(raw_pty, mallee.r)
mallee.r$rawclass <- factor(mallee.r$rawclass)
contrasts(mallee.r$rawclass)  <- cbind(c(-1,0,0,0,1), c(-1,0,0,1,0), c(-1,0,1,0,0), c(-1,1,0,0,0)) #contr.treatment(5)

lm01 <- lm(Moisture~rawclass, data=mallee.r)
anova(lm01)
summary(lm01)

minraw2.ct <- as.simpleparty(minraw.ct)
plot(minraw2.ct)

with(mallee.r, tapply(Moisture, rawclass, mean, na.rm=T))
