require(rpart)
require(DAAG)
require(partykit)
require(rpart.plot)
require(rattle)

# calculate readil;y avaialble water
Soils$raw1 <- with(Soils, (KPa8-KPa40)*1000)
Soils$raw2 <- with(Soils, (KPa8-KPa60)*1000)
Soils$taw  <- with(Soils, (KPa8-KPa1500)*1000)
str(Soils)

# store the number of rows in the mallee dataframe
nrows <- nrow(Soils)

set.seed(632087)
# randomly sample 4 fifths of dataset without replacement
for (i in 1:2500){
  rows <- sample(1:nrows, floor(0.8*nrows))
  train <- Soils[rows, c('raw2','Field.CO3', 'Field.Texture')]
  test <-  Soils[-rows, c('raw2','Field.CO3', 'Field.Texture')]

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
    minsraw.ct <- raw.ct
    min.i <- 1
    }
    
  # save regression tree information in min & max variables as appropriate
    else {
    if (sumsq < minsumsq) {
      minsumsq <- sumsq
      mintrain <- train
      mintest <- test
      minsraw.ct <- raw.ct
      min.i <- i
      }
    }
  }

# print out minimum mean squares
cat("Minimum mean square", minsumsq, " at iteration", min.i, "\n")

# plot of minimum conditional partitioned tree 
plot(minsraw.ct, main="Minimum Mean Square Tree")

# save terminal nodes names to dataframe
nodes <- predict(minsraw.ct, newdata=Soils, type="node")
Soils$rawclass <- ifelse(nodes==2, "Node02", 
  ifelse(nodes==4, "Node04", 
    ifelse(nodes==6, "Node06","Node07")))
Soils$rawclass <- factor(Soils$rawclass)
str(Soils)

# extract and print mean Readily Available Water for each terminal node
head(Soils[, c(10,11,19,20)], 10)
soils.raw <- with(Soils, tapply(raw2, rawclass, mean, na.rm=T))
soils.rawsd <- with(Soils, tapply(raw2, rawclass, sd, na.rm=T))

str(soils.raw)
# Are each node mean different
# contrasts are treatment contrasts
contrasts(Soils$rawclass) <- cbind(c(0,1,0,0), c(0,0,1,0), c(0,0,0,1))
options(contrasts=c("contr.treatment","contr.helmert"))
contrasts(Soils$rawclass)
lm01 <- lm(raw2~rawclass, data=Soils)
anova(lm01)
summary(lm01)

raw_sp1 <- partysplit(2L, breaks=3)
raw_sp2 <- partysplit(3L, breaks = 1)
raw_sp3 <- partysplit(3L, breaks = 5)
raw_sp4 <- partysplit(3L, breaks = 6)

raw_node <- partynode(1L, split=raw_sp1, kids=list(
  partynode(2L, split=raw_sp2, kids=list(
    partynode(3L, info="60mm"),
    partynode(4L, info="74mm"))),
  partynode(5L, split=raw_sp3, kids=list(
    partynode(6L, info="81mm"),
    partynode(7L, split=raw_sp4, kids=list(
      partynode(8L, info="67mm"),
      partynode(9L, info="54mm")))))))
# extract subset of mallee dataframe, rename variables and rename Reaction levels
# thenrefactor
Soils.r <- Soils[, c('raw2','Field.CO3', 'Field.Texture')] 
names(Soils.r) <- c('Moisture','Reaction','Texture')
Soils.r$Reaction <- with(Soils.r, ifelse(Reaction=='N','Nil', 
  ifelse(Reaction=='S', 'Slight',
    ifelse(Reaction=='M', 'Medium',
      ifelse(Reaction=='H', 'High','Very High')))))
Soils.r$Reaction <- factor(Soils.r$Reaction, levels=c('Nil','Slight','Medium','High','Very High'), ordered=TRUE)

# rename Texture and refactor
Soils.r$Texture <- with(Soils.r, ifelse(Texture=='S','Sand', 
  ifelse(Texture=='LS', 'Loamy Sand',
    ifelse(Texture=='SL', 'Sandy Loam',
      ifelse(Texture=='LSCL', 'Light Sandy Clay Loam',
        ifelse(Texture=="SCL", 'Sandy Clay Loam',
          ifelse(Texture=='CL', 'Clay Loam', 'Clay')))))))
Soils.r$Texture <- factor(Soils.r$Texture, levels=c('Sand','Loamy Sandy','Sandy Loam','Light Sandy Clay Loam','Sandy\n Clay Loam','Clay Loam','Clay'), ordered=TRUE)
str(Soils.r) 

raw_pty <- party(raw_node, Soils.r)
print(raw_pty, terminal_panel=function(n) c(", Readily Available Water is ", n$info))
plot(raw_pty, terminal_panel=node_terminal(raw_pty, fill="light grey"), inner_panel=node_inner(raw_pty, fill="grey"))
