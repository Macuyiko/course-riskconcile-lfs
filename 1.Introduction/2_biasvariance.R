
data <- data.frame(x=c(0.5, 1), y=c(1, 3))

model <- lm(y ~ x, data)

model
#Call:
#  lm(formula = y ~ x, data = data)
#
#Coefficients:
#  (Intercept)            x  
#-1            4  

summary(model)
#Call:
#  lm(formula = y ~ x, data = data)
#
#Residuals:
#  ALL 2 residuals are 0: no residual degrees of freedom!
#  
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)       -1         NA      NA       NA
#x                  4         NA      NA       NA
#
#Residual standard error: NaN on 0 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:    NaN 
#F-statistic:   NaN on 1 and 0 DF,  p-value: NA

plot(data$x, data$y, xlim=c(.4, 1.2), ylim=c(0, 4), pch=16)
abline(model)

# Zero bias... but what about variance?

# Randomly mutate the data set a little and plot predictions
newdata <- data.frame(x=c(runif(5, 0.3, 0.7), runif(5, 0.8, 1.2)),
                      y=c(runif(5, 0.7, 1.3), runif(5, 2, 4)))
predicted <- predict(model, newdata)
points(newdata$x, newdata$y, col='red')
segments(newdata$x, newdata$y, newdata$x, predicted, col='red')

# Refit the model on pairs of new data points?
for (n in 1:5) {
  model <- lm (y ~ x, newdata[c(n,n+5),])
  abline(model, col=rgb(1, 0, 0, 0.5))
}


