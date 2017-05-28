require(tidyverse)
require(rpart)
require(rpart.plot)

str(mallee)

mall_tree <- mallee %>% select(raw60, Field.Texture, Field.CO3)
head(mall_tree)

# Set minsplit = 10 to fit every data point
full_fit <- rpart(raw60 ~ Field.Texture + Field.CO3, data = mall_tree, minsplit = 2, method='anova')
prp(full_fit)

# To help validate our hyperparameter combinations, we’ll split our data into
# training and test sets (in an 80/20 split):

set.seed(1111)
n <- nrow(mall_tree)
train_rows <- sample(seq(n), size = .8 * n)
train <- mall_tree[ train_rows, ]
test  <- mall_tree[-train_rows, ]

# Step one for grid search is to define our hyperparameter combinations. Say we want
# to test a few values for minsplit and maxdepth. I like to setup the grid of their
# combinations in a tidy data frame with a list and cross_d as follows:

# Define a named list of parameter values
gs <- list(minsplit = c(2, 10, 20), maxdepth = c(1, 3, 5)) %>%  cross_d() # Convert to data frame grid

gs

#Note that the list names are the names of the hyperparameters that we want to
# adjust in our model function.

# Create a model function

#We’ll be iterating down the gs data frame to use the hyperparameter values in a
# rpart model. The easiest way to handle this is to define a function that accepts a
# row of our data frame values and passes them correctly to our model. Here’s what

# I’ll use:
mod <- function(...) {
  rpart(raw60 ~ Field.Texture + Field.CO3, data = train, control = rpart.control(...))
}

# Notice the argument ... is being passed to control in rpart, which is where these
# hyperparameters can be used.

#Now, to fit our models, use pmap to iterate down the values. The following is
# iterating through each row of our gs data frame, plugging the hyperparameter
# values for that row into our model.

gs <- gs %>% mutate(fit = pmap(gs, mod))
gs

# Obtain accuracy

# Next, let’s assess the performance of each fit on our test data. To handle this
# efficiently, let’s write another small function:

compute_accuracy <- function(fit, test_features, test_labels) { 
  predicted <- predict(fit, test_features, type = "class")
  mean(predicted == test_labels)
}

#Now apply this to each fit:
test_features <- test %>% select(-raw60)
test_labels   <- test$raw60

gs <- gs %>%
  mutate(test_accuracy = map_dbl(fit, compute_accuracy,
                                 test_features, test_labels))
gs
