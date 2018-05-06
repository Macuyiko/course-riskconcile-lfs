library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(e1071)
library(klaR)

data <- read.csv('BayesData.csv')

ggplot(data, aes(x=ROE, y=DebtEquity, color=Default, size=Age)) + geom_point()

model <- naiveBayes(Default ~ ., data)

# Using another package
model <- NaiveBayes(Default ~ ., data)
model
plot(model)

# Draw contour lines
test_data <- expand.grid(Age=c(3), DebtEquity=seq(0,4,0.1), ROE=seq(0,0.20,0.01))
predictions <- predict(model, test_data)
test_data %<>% bind_cols(as.data.frame(predictions$posterior))
test_data$cat <- test_data$yes > 0.5
test_data$prob <- ifelse(test_data$yes > 0.5, test_data$yes, test_data$no)

ggplot() +
  geom_point(data=data, aes(x=ROE, y=DebtEquity, size=Age, color=Default)) +
  stat_contour(data=test_data, aes(x=ROE, y=DebtEquity, z=prob, color=cat), binwidth=0.01)
  

# Alternative packages: mlr and naivebayes

