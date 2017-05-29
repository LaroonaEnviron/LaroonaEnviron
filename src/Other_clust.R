require(cluster)
require(factoextra)

# fuzzy clustering
mall.fuzzy <- fanny(mall_sc, 10)
head(mall.fuzzy$membership, 10)

mall.fuzzy$coeff # Dunn's partition coefficient

head(mall.fuzzy$clustering, 20)

# visualize
fviz_cluster(list(data=mall_sc, cluster=mall.fuzzy$clustering),
  palette = "",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.2, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6
  )

# model based clustering
require(ggpubr)
ggscatter(mallee, x='Clay', y='raw60', fill="red", pallette="Set1", mean.point=TRUE, color="Field.CO3") + geom_density2d(show.legend=TRUE)

require(mclust)
mall.modc <- Mclust(mall_sc)
summary(mall.modc)

# Hierarchical kmeans clustering
require(factoextra)
mallee.hk <- hkmeans(mall_sc, 7)
names(mallee.hk)

mallee.hk # print results

# Visualize the tree
fviz_dend(mallee.hk, cex = 0.6, palette = "jco",
    rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# visualize clusters
fviz_cluster(list(data=mall_sc, cluster=mallee.hk$cluster), 
  palette = "uchicago",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.3, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6,
  main="Heirarchical Cluster - kmeans"
  )

# conditional regression trees
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

for (n in 1:1) {
  set.seed(seed[n])
#  mallee$counts <- 0

  for (i in 1:2500){
    rows <- sample(1:nrows, floor(0.8*nrows))
    train <- mallee[rows, c('raw60','Field.CO3', 'Field.Texture')]
    test <-  mallee[-rows, c('raw60','Field.CO3', 'Field.Texture')]

# derive regression tree
    raw60.ct <- ctree(raw60 ~ Field.Texture + Field.CO3, data=train, control=ctree_control(teststat="quad", testtype="Bonferroni", mincriterion=0.97))
    test$pred <- predict(raw60.ct, newdata=test)
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
      minraw60.ct <- raw60.ct
      min.i <- 1
      }
    
  # save regression tree information in min & max variables as appropriate
      else {
      if (sumsq < minsumsq) {
        minsumsq <- sumsq
        mintrain <- train
        mintest <- test
        minraw60.ct <- raw60.ct
        min.i <- i
        }
      }
    }

# print out minimum mean squares
  minss[n] <- minsumsq  
  cat("Minimum sum of squares=", minss[n], " for seed[",n,"]=",seed[n], "\n")
  plot(minraw60.ct, main=paste("Minimum Mean Square Tree for seed[",n,"] = ",seed[n]))
  #pause()
}


plot(1:10, minss, type='b', pch=16, col="red", xlab="Sample No.", ylab="Mean Sum of Squares")
text(1:10, minss, labels=seed)

# save terminal nodes names to dataframe
nodes <- predict(minraw60.ct, newdata=mallee, type="node")
mallee$rawclass <- ifelse(nodes==3, "Node03", 
  ifelse(nodes==4, "Node04", 
    ifelse(nodes==6, "Node06",
      ifelse(nodes==8, "Node08", "Node09"))))
mallee$rawclass <- factor(mallee$rawclass)
str(mallee)

# visualize clusters
fviz_cluster(list(data=mallee[, -c(3, 6, 9, 10, 14:20)], 
  cluster=as.integer(mallee$rawclass)), 
  palette = "uchicago",
  ellipse.type = "convex", # Concentration ellipse
  ellipse.alpha = 0.3, # transparency
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal(), 
  labelsize = 6,
  main="Conditional Regression Trees"
  )
str(mallee)