library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(rpart)
library(randomForest)
library(readxl)

data <- read_excel('TreeData.xls') %>% as_tibble
# Make sure the target is a categorical! Otherwise regression will be performed
data$Y <- factor(data$Y)

ggplot(data, aes(X1, X2, color=Y)) + geom_point() + theme_bw()

# A classification tree
model <- rpart(Y ~ ., data)

model
plot(model)
# Error in plot.rpart(model) : fit is not a tree, just a root

model <- rpart(Y ~ ., data, control = rpart.control(minsplit=1, minbucket=1, cp = 0))
plot(model); text(model)
library(rpart.plot)
rpart.plot(model)
model

# Plot the decision contours
test_data <- expand.grid(X1=seq(0,1,0.01), X2=seq(0,1,0.01))
test_data$predictions <- predict(model, test_data, type="prob")[,2]
test_data$outcome <- factor(as.numeric(test_data$predictions > 0.5))

ggplot() + 
  geom_point(data=test_data, aes(X1, X2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(X1, X2, fill=Y), color='black', size=5, shape=21) +
  theme_bw()

# Using a less deep tree
model <- rpart(Y ~ ., data, control = rpart.control(minsplit=1, minbucket=1, cp = 0.5))
rpart.plot(model)

test_data$predictions <- predict(model, test_data, type="prob")[,2]
test_data$outcome <- factor(as.numeric(test_data$predictions > 0.5))

ggplot() + 
  geom_point(data=test_data, aes(X1, X2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(X1, X2, fill=Y), color='black', size=5, shape=21) +
  theme_bw()

# With randomForest -- note that the confusion matrix is based on OOB
model <- randomForest(Y ~ ., data)
model

test_data$predictions <- predict(model, test_data, type="prob")[,2]
test_data$outcome <- factor(as.numeric(test_data$predictions > 0.5))

ggplot() + 
  geom_point(data=test_data, aes(X1, X2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(X1, X2, fill=Y), color='black', size=5, shape=21) +
  theme_bw()

p <- factor(as.numeric(predict(model, data, type="prob")[,2] > 0.5))
table(data$Y, p)

library(verification)
roc.plot(as.numeric(as.character(data$Y)), predict(model, data, type="prob")[,2])

test_data$predictions <- predict(model, test_data, type="prob")[,2]
test_data$outcome <- factor(as.numeric(test_data$predictions > 0.9))

ggplot() + 
  geom_point(data=test_data, aes(X1, X2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(X1, X2, fill=Y), color='black', size=5, shape=21) +
  theme_bw()

importance(model)
partialPlot(model, x.var='X2', pred.data=as.data.frame(data), which.class='1')
