library(dplyr)
library(tidyr)
library(e1071)
library(ggplot2)

x1 <- c(rnorm(100, 0, 0.5), rnorm(10, -1.5, .1))
x2 <- c(rnorm(100, 0, 0.5), rnorm(10,  1.5, .1))
y <- factor(c(rep(0, 100), rep(1, 10)))

data <- data.frame(x1=x1, x2=x2, y=y)
ggplot() + geom_point(data=data, aes(x1, x2, color=y, fill=y))
  
model <- svm(y ~ ., data, probability=TRUE)

test_data <- expand.grid(x1=seq(-2,1.5,.01), x2=seq(-1,2,0.01))
test_data$predictions <- predict(model, test_data, probability=TRUE) %>% attr('probabilities') %>% .[,2]
test_data$predictions %>% hist
test_data$outcome <- factor(as.numeric(test_data$predictions > 0.5))

ggplot() + 
  geom_point(data=test_data, aes(x1, x2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(x1, x2, fill=y), color='black', size=5, shape=21) +
  theme_bw()

# Tuning
tuned <- tune(svm, y ~ ., data=data, probability=TRUE, cross=5, nrepeat=10,
     ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 50 ), 
                   gamma = c(.0001, .001, .01, .1, 1, 5, 10)))

test_data <- expand.grid(x1=seq(-2,1.5,.01), x2=seq(-1,2,0.01))
test_data$predictions <- predict(tuned$best.model, test_data, probability=TRUE) %>% attr('probabilities') %>% .[,2]
test_data$predictions %>% hist
test_data$outcome <- factor(as.numeric(test_data$predictions > 0.5))

ggplot() + 
  geom_point(data=test_data, aes(x1, x2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(x1, x2, fill=y), color='black', size=5, shape=21) +
  theme_bw()

# Unsupervised SVM
model <- svm(x=data[,c('x1', 'x2')], y=NULL, nu=0.50, type='one-classification')

test_data <- expand.grid(x1=seq(-2,1.5,.01), x2=seq(-1,2,0.01))
test_data$outcome <- factor(as.numeric(predict(model, test_data)))

ggplot() + 
  geom_point(data=test_data, aes(x1, x2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(x1, x2, fill=y), color='black', size=5, shape=21) +
  theme_bw()

# Comparison with iForest
library(isofor)
model <- iForest(data[,c('x1', 'x2')], nt=100, phi=25)
test_data <- expand.grid(x1=seq(-2,1.5,.01), x2=seq(-1,2,0.01))
test_data$outcome <- predict(model, test_data)

ggplot() + 
  geom_point(data=test_data, aes(x1, x2, color=outcome, fill=outcome), shape=15, size=3) + 
  geom_point(data=data, aes(x1, x2, fill=as.numeric(y)), color='black', size=5, shape=21) +
  theme_bw()
