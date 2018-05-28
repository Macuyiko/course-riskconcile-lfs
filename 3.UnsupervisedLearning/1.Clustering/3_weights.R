data <- read.csv('Cluster.csv')

# Perform k-means
cl <- kmeans(data[,2:3], centers=2)
cl

plot(data$S, data$Spread, col=cl$cluster)

# Standard kmeans object cannot perform prediction, but flexclust can
library(flexclust)

cl <- as.kcca(cl, data=data[,2:3])
cl@cluster
plot(data$S, data$Spread, col=cl@cluster)

# Now we can use it
pred <- predict(cl, data.frame(S=8, Spread=1))
points(8, 1, pch=16, col=pred)

# Now let's do the same with attribute weighting
weights <- apply(data[,2:3], 2, function(x) 2*var(x))
# Some annoying transposes to make R divide column-wise
scaled <- as.data.frame(t( t(data[,2:3]) / weights ))
plot(scaled$S, scaled$Spread, main='Reweighted')

cl <- kmeans(scaled, centers=2)
cl <- as.kcca(cl, data=scaled)
plot(data$S, data$Spread, col=cl@cluster)

new_scaled <- data.frame(S=8/weights[1], Spread=1/weights[2])

pred <- predict(cl, new_scaled)
points(8, 1, pch=16, col=pred)
