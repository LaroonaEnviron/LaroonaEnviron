# store the number of rows in the mallee dataframe
nrows <- nrow(soils)

# test using conditional partioning
set.seed(632087)  # this seed gave the lowest withinss of over 100 random seeds tested.

# randomly sample 80% of dataset without replacement
for (i in 1:2500){
  rows <- sample(1:nrows, floor(0.8*nrows))
  train <- soils[rows, c('raw60','Field.CO3', 'Field.Texture')]
  test <-  soils[-rows, c('raw60','Field.CO3', 'Field.Texture')]

# derive regression tree
  raw.ct <- ctree(raw60 ~ Field.Texture + Field.CO3, data=train, control=ctree_control(teststat="quad", testtype="Bonferroni", mincriterion=0.97))
  test$pred <- predict(raw.ct, newdata=test)
  Raw.tab <- with(test, tapply(raw60, list(Field.Texture, Field.CO3), mean, na.rm=T))
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
soils.ct <- minraw.ct

# save terminal nodes names to dataframe
nodes <- predict(soils.ct, newdata=soils, type="node")
soils$rawclass <- ifelse(nodes==2, "Node02", 
  ifelse(nodes==4, "Node04", 
    ifelse(nodes==6, "Node06","Node07"
    )))
soils$rawclass <- factor(soils$rawclass)
#with(soils, tapply(raw60, rawclass, mean, na.rm=T))

car_sp1 <- partysplit(2L, breaks=3)
raw_sp2 <- partysplit(3L, breaks = 5)
raw_sp3 <- partysplit(3L, breaks = 6)

raw_node <- partynode(1L, split=car_sp1, kids=list(
    partynode(2L, info = "60mm"),
    partynode(3L, split=raw_sp2, kids=list(
      partynode(4L, info ="78mm"),
    partynode(5L, split=raw_sp3, kids=list(
      partynode(6L, info = "67mm"),
      partynode(7L, info = "54mm")))))))

# extract subset of soils dataframe, rename variables and rename Reaction levels
# then refactor
soils.r <- soils[, c('raw60','Field.CO3', 'Field.Texture', 'rawclass')] 
names(soils.r) <- c('Moisture','Reaction','Texture','RAW')
soils.r$Reaction <- with(soils.r, ifelse(Reaction=='N','Nil', 
  ifelse(Reaction=='S', 'Slight',
    ifelse(Reaction=='M', 'Medium',
      ifelse(Reaction=='H', 'High','Very High')))))
soils.r$Reaction <- factor(soils.r$Reaction, levels=c('Nil','Slight','Medium','High','Very High'), ordered=TRUE)

# rename Texture and refactor
soils.r$Texture <- with(soils.r, ifelse(Texture=='S','Sand', 
  ifelse(Texture=='LS','Loamy Sand',
    ifelse(Texture=='SL', 'Sandy Loam',
      ifelse(Texture=='LSCL', 'Light Sandy Clay Loam',
        ifelse(Texture=="SCL", 'Sandy Clay Loam',
          ifelse(Texture=='CL', 'Clay Loam', 'Clay')))))))
soils.r$Texture <- factor(soils.r$Texture, levels=c('Sand','Loamy Sand','Sandy Loam','Light Sandy Clay Loam','Sandy Clay Loam','Clay Loam','Clay'), labels=c('Sand','Loamy Sand','Sandy Loam','Light Sandy\n Clay Loam','Sandy\n Clay Loam','Clay Loam','Clay'), ordered=TRUE)

# rename RAW and refactor
soils.r$RAW <- with(soils.r, ifelse(RAW=="Node02", "RAW01",
   ifelse(RAW=="Node04", "RAW02",
     ifelse(RAW=="Node06", "RAW03", "RAW04"))))
     
soils.r$RAW <- factor(soils.r$RAW, ordered=TRUE)

raw_pty <- party(raw_node, soils.r)
plot(raw_pty, terminal_panel=node_terminal(raw_pty, fill="light blue"), inner_panel=node_inner(raw_pty, fill="grey"))

aveRaw01 <- with(soils.r, tapply(Moisture, RAW, mean, na.rm=T))
sdRaw01  <- with(soils.r, tapply(Moisture, RAW, sd, na.rm=T))
contrasts(soils.r$RAW) <- cbind(c(-3,1,1,1), c(0,-2, 1,1), c(0,0,-1,1))
soils.lm01 <- lm(Moisture~RAW, data=soils.r)
fstats.soils <- summary(soils.lm01)$'fstatistic'
