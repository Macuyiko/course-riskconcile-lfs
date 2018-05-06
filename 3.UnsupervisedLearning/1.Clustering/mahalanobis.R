data <- read.csv('Cluster.csv')

# Hierarchical clustering: distance matrix based

mah <- function(x, S = NULL) {
  if(is.null(S)) S <- cov(x)
  # Apply mahalanobis for every row in the data set
  out <- lapply(1:nrow(x), function(i) {
    mahalanobis(x=x, center=do.call("c", x[i, ]), cov=S)
  })
  return(as.dist(do.call("rbind", out)))
}

d <- mah(data[,2:3])
cl <- hclust(d, method="average")

plot(cl)
cl_cut <- cutree(cl, 6)
plot(data$S, data$Spread, col=cl_cut, pch=16)

# Rescaling based to convert to Euclidian k-means

x <- as.matrix(data[,2:3])
C <- chol( var(x) ) # Choleski Decomposition of the variance matrix
y <- x %*% solve(C)
var(y) # Is now the identity matrix (or close enough) -- so we can apply k-means on y

cl <- kmeans(y, 6)
plot(data$S, data$Spread, col=cl$cluster, pch=16) # Same outcome!

# Comparison with normal k-means

cl <- kmeans(x, 6)
plot(data$S, data$Spread, col=cl$cluster, pch=16) # Same outcome!
