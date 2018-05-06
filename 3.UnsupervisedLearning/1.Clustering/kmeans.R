data <- read.csv('Cluster.csv')

head(data)
head(data[,2:3])

X <- log(data$S[1]/data$S)
Y <- log(data$Spread/data$Spread[1])
model <- lm(Y ~ 0 + X)

model
# Coef: 3.084 

summary(model)
# Multiple R-squared:  0.9482,	Adjusted R-squared:  0.9479

# Plot transformed data and regression line
plot(X, Y)
abline(model)

# Plot regression line in original feature space using a piece of test data
X_t <- seq(5,12,0.1)
Y_t <- data$Spread[1] * exp(coef(model)[1] * log(data$S[1]/X_t))
plot(data$S, data$Spread)
lines(X_t, Y_t)

# Perform k-means
cl <- kmeans(data[,2:3], centers=2)
cl

plot(data$S, data$Spread, col=cl$cluster)
lines(X_t, Y_t, lwd=2)

# How many clusters?
plot(sapply(2:20, function(k) kmeans(data[,2:3], centers=k)$tot.withinss), type='l', 
     ylab='Total SSE', xlab='k')

# Let's continue with two clusters and create two fits
model1 <- lm(Y[cl$cluster == 1] ~ 0 + X[cl$cluster == 1])
Y_t1 <- data$Spread[1] * exp(coef(model1)[1] * log(data$S[1]/X_t))

model2 <- lm(Y[cl$cluster == 2] ~ 0 + X[cl$cluster == 2])
Y_t2 <- data$Spread[1] * exp(coef(model2)[1] * log(data$S[1]/X_t))

plot(data$S, data$Spread, col=cl$cluster)
lines(X_t, Y_t1, lwd=2, col=1)
lines(X_t, Y_t2, lwd=2, col=2)


