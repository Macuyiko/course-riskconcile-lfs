# Let's create an example data set
X <- data.frame(x1=rnorm(10), x2=rnorm(10))

# As well as a bumped data set
Z <- data.frame(x1=X$x1, x2=X$x2)
Z[1,1] <- Z[1,1]+2
Z[1,2] <- Z[1,2]+3

# For a new point we have to consider how close it is to the overal set of observations
Y <- c(3, 3)

# The Mahalanobis distance is calculated against both data-sets

distance_to_X <- sqrt(sum(mahalanobis(x=X, center=Y, cov=cov(X))))
distance_to_Z <- sqrt(sum((mahalanobis(x=Z, center=Y, cov=cov(Z)))))

# Robust covariances
library(robustbase)
distance_to_X_rob <- sqrt(sum(mahalanobis(x=X, center=Y, cov=covMcd(X)$cov)))
distance_to_Z_rob <- sqrt(sum(mahalanobis(x=Z, center=Y, cov=covMcd(Z)$cov)))

plot(Z, pch=16, xlim=c(-1, 4), ylim=c(-1, 4), main=paste('Distance to Z:', distance_to_Z_rob))
points(3, 3, pch=3, col=2)
points(X, pch=16, col=3)

