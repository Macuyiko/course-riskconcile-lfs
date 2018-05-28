library(caret)
library(verification)

data <- data.frame(x1=c(runif(10, 1, 3), runif(5, 5, 6)),
                   x2=c(runif(10, 0, 1), runif(5, 3, 6)),
                   y=c(rep(0, 10), rep(1, 5)))

plot(identity, type='n', xlab='x1', ylab='x2', xlim=c(0, 6), ylim=c(0, 6))
points(data$x1, data$x2, pch=16, col=ifelse(data$y==0, 'blue', 'red'))

# How would you 'classify' this data set?

model <- glm(factor(y) ~ ., data=data, family=binomial)

slope <- coef(model)[2]/(-coef(model)[3])
intercept <- coef(model)[1]/(-coef(model)[3]) 
abline(intercept, slope)

# With caret...

model_caret <- train(y ~ .,  data=data, method="glm", family="binomial")

#Generalized Linear Model 
#15 samples
#2 predictor
#No pre-processing
#Resampling: Bootstrapped (25 reps) 
#Summary of sample sizes: 15, 15, 15, 15, 15, 15, ... 
#Resampling results:
#
#  RMSE         Rsquared   MAE         
#0.001352159  0.9999598  0.0005791499

model_caret$finalModel

predicted <- ifelse(predict(model_caret) >= 0.5, 1, 0)
confusionMatrix(predicted, data$y)

#Confusion Matrix and Statistics
#           Reference
#Prediction  0  1
#         0 10  0
#         1  0  5
#Accuracy : 1         
#95% CI : (0.782, 1)
#No Information Rate : 0.6667    
#P-Value [Acc > NIR] : 0.002284  

roc.plot(data$y, predict(model_caret))

# A harder example...

data <- data.frame(x1=c(runif(10, 1, 3), runif(5, 5, 6)),
                   x2=c(runif(10, 0, 1), runif(5, 3, 6)),
                   y=c(rep(0, 6), rep(1, 7), rep(0, 2)))

plot(identity, type='n', xlab='x1', ylab='x2', xlim=c(0, 6), ylim=c(0, 6))
points(data$x1, data$x2, pch=16, col=ifelse(data$y==0, 'blue', 'red'))

model_caret <- train(y ~ .,  data=data, method="glm", family="binomial")
predicted <- ifelse(predict(model_caret) >= 0.5, 1, 0)
confusionMatrix(predicted, data$y)

slope <- coef(model_caret$finalModel)[2]/(-coef(model_caret$finalModel)[3])
intercept <- coef(model_caret$finalModel)[1]/(-coef(model_caret$finalModel)[3]) 
abline(intercept, slope)

roc.plot(data$y, predict(model_caret))


