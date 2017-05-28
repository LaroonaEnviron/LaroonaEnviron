require(rpart)
require(DAAG)
require(partykit)
require(rpart.plot)
require(rattle)

# store the number of rows in the mallee dataframe
nrows <- nrow(mallee)
minss <- vector(mode="double", length=10)
seed <- vector(mode="integer", length=10)

# test using conditional partioning
# randomly sample 4 fifths of dataset without replacement
# generate a vector of 10 random seeds with the first seed as 632087
seed[1] <- 632087
set.seed(632087)

for (n in 2:10) seed[n] <- floor(runif(1, min=1, max=999999))

for (n in 1:10) {
  set.seed(seed[n])
#  mallee$counts <- 0

  for (i in 1:2500){
    rows <- sample(1:nrows, floor(0.8*nrows))
    train <- mallee[rows, c('taw','Field.CO3', 'Field.Texture')]
    test <-  mallee[-rows, c('taw','Field.CO3', 'Field.Texture')]
#    mallee$counts[rows] <- ifelse(rows, mallee$counts[rows]+1,)
  
# derive regression tree
    taw.ct <- ctree(taw ~ Field.Texture + Field.CO3, data=train, control=ctree_control(teststat="quad", testtype="Bonferroni", mincriterion=0.97))
    test$pred <- predict(taw.ct, newdata=test)
    Raw.tab <- with(test, tapply(taw, list(Field.Texture, Field.CO3), mean, na.rm=T))
    pred.tab <- with(test, tapply(pred, list(Field.Texture, Field.CO3), mean, na.rm=T))
    Nobs= sum(!is.na(Raw.tab-pred.tab))
    sumsq <- sum((Raw.tab-pred.tab)^2, na.rm=T)
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

# print out minimum mean squares
  minss[n] <- minsumsq  
  cat("Minimum sum of squares=", minss[n], " for seed[",n,"]=",seed[n], "\n")
  plot(mintaw.ct, main=paste("Minimum Mean Square Tree for seed[",n,"] = ",seed[n]))
  #pause()
}

plot(1:10, minss, type='b', xlab="Sample No.", ylab="Mean Sum of Squares")

#----------------------------------------------------------------
# test on total available water
seed[1] <- 407885
set.seed(407885)

for (n in 2:10) seed[n] <- floor(runif(1, min=1, max=999999))

for (n in 1:10) {
  set.seed(seed[n])
#  mallee$counts <- 0

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

# print out minimum mean squares
  minss[n] <- minsumsq  
  cat("Minimum sum of squares=", minss[n], " for seed[",n,"]=",seed[n], "\n")
  plot(mintaw.ct, main=paste("Minimum Mean Square Tree for seed[",n,"] = ",seed[n]))
  }

plot(1:10, minss, type='b', xlab="Sample No.", ylab="Mean Sum of Squares")

# save terminal nodes names to dataframe
nodes <- predict(mintaw.ct, newdata=mallee, type="node")
mallee$tawclass <- ifelse(nodes==3, "Node03", 
  ifelse(nodes==4, "Node04", 
    ifelse(nodes==7, "Node07",
      ifelse(nodes==8, "Node08",
        ifelse(nodes==10,"Node10","node11")))))
mallee$tawclass <- factor(mallee$tawclass)
str(mallee)

# extract and print mean Readily Availabe Water for each terminal node
head(mallee[, c(10,11,19,20:23)], 10)
with(mallee, tapply(taw, tawclass, mean, na.rm=T))

# contrasts are treatment contrasts
contrasts(mallee$tawclass) <- cbind(c(-5,1,1,1,1,1), c(0,-4,1,1,1,1), c(0,0,-3,1,1,1), c(0,0,0,-2,1,1), c(0,0,0,0,-1,1))
#options(contrasts=c("contr.treatment","contr.helmert"))
contrasts(mallee$tawclass)
taw.lm01 <- lm(taw~tawclass, data=mallee)
anova(taw.lm01)
summary(taw.lm01)

taw_sp1 <- partysplit(3L, index = 1:2)
taw_sp2 <- partysplit(3L, breaks = 1)
taw_sp3 <- partysplit(2L, breaks = 3)
taw_sp4 <- partysplit(3L, breaks = 4)
taw_sp5 <- partysplit(2L, breaks = 4)

taw_node <- partynode(1L, split=taw_sp1, kids=list(
  partynode(2L, split=taw_sp2, kids=list(
    partynode(3L, info="63mm"),
    partynode(4L, info="87mm"))),
  partynode(5L, split=taw_sp3, kids=list(
    partynode(6L, split=taw_sp4, kids=list(
      partynode(7L, info="116mm"),
      partynode(8L, info="131mm"))),
    partynode(9L, split=taw_sp5, kids=list(
      partynode(10L, info="140mm"),
      partynode(11L, info="152mm")))))))

# subset mallee for a reduced mallee dataframe
mallee.t <- mallee[, c('taw','Field.CO3', 'Field.Texture', 'tawclass')] 
names(mallee.t) <- c('Moisture','Reaction','Texture','Class')
mallee.t$Reaction <- with(mallee.t, ifelse(Reaction=='N','Nil', 
  ifelse(Reaction=='S', 'Slight',
    ifelse(Reaction=='M', 'Medium',
      ifelse(Reaction=='H', 'High','Very High')))))
mallee.t$Reaction <- factor(mallee.t$Reaction, levels=c('Nil','Slight','Medium','High','Very High'), ordered=TRUE)

# rename Texture and refactor
mallee.t$Texture <- with(mallee.t, ifelse(Texture=='S','Sand', 
  ifelse(Texture=='LS', 'Loamy Sand',
    ifelse(Texture=='SL', 'Sandy Loam',
      ifelse(Texture=='LSCL', 'Light Sandy Clay Loam',
        ifelse(Texture=='SCL', 'Sandy Clay Loam',
          ifelse(Texture=='CL', 'Clay Loam', 'Clay')))))))
mallee.t$Texture <- factor(mallee.t$Texture, levels=c('Sand','Loamy Sand','Sandy Loam','Light Sandy Clay Loam','Sandy Clay Loam','Clay Loam','Clay'), labels=c('Sand','Loamy Sand','Sandy Loam','Light Sandy\n Clay Loam','Sandy\n Clay Loam','Clay Loam','Clay'), ordered=TRUE)

# rename Class and refactor
mallee.t$Class <- with(mallee.t, ifelse(Class=="Node03", "TAW01",
   ifelse(Class=="Node04", "TAW02",
     ifelse(Class=="Node07", "TAW03",
       ifelse(Class=="Node08", "TAW04",
         ifelse(Class=="Node10", "TAW05", "TAW06"))))))
mallee.t$Class <- factor(mallee.t$Class, ordered=TRUE)

str(mallee.t)
head(mallee.t, 10)

taw_pty <- party(taw_node, mallee.t)
print(taw_pty, terminal_panel=function(n) c(", Readily Available Water is ", n$info))
plot(taw_pty, terminal_panel=node_terminal(taw_pty, fill="light blue"), inner_panel=node_inner(taw_pty, fill="grey"))

aveTaw <- with(mallee.t, tapply(Moisture, Class, mean, na.rm=T))
sdTaw  <- with(mallee.t, tapply(Moisture, Class, sd, na.rm=T))
