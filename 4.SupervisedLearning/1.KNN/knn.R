x1 <- c(rnorm(50, 1, 1), rnorm(50, 4, .8), rnorm(50, 3, 1))
x2 <- c(rnorm(50, 0, 1.2), rnorm(50, 2, .5), rnorm(50, 5, 1))
y  <- c(rep(1, 50), rep(2, 50), rep(3, 50))

data <- data.frame(x1, x2, y)

plot(data$x1, data$x2, col=data$y)

# New instance to predict
new <- c(3, 2)
points(new[1], new[2], col='orange', pch=16, cex=3)

# Hand-rolled prediction function
knn.predict <- function(instance, data, k=3) {
  n <- data.frame(t(instance))
  colnames(n) <- colnames(data[,1:2])
  d <- dist(bind_rows(data[,1:2], n)) %>% as.matrix
  idx <- names(sort(d[nrow(d),])[2:(k+1)])
  s <- sort(table(data[idx,]$y), decreasing=T)
  predicted <- as.numeric(names(s)[1])
  prob <- s[1] / sum(s)
  list(prediction=predicted, probability=prob, neighbors=idx)
}

# Let's try it
p <- knn.predict(new, data, k=10)
points(new[1], new[2], col=p$prediction, pch=16, cex=2)
segments(rep(new[1], 3), rep(new[2], 3),
         data[p$neighbors,1],data[p$neighbors,2])

# Draw decision boundary
test_data <- expand.grid(x1=seq(-1, 6, 0.1), x2=seq(-3, 7, 0.1))
apply(test_data, 1, function(x) points(x[1], x[2], col=knn.predict(x, data)$prediction, pch=15, cex=2))
points(data$x1, data$x2, pch=21, col='white', bg=data$y)

# And for k = 10?
apply(test_data, 1, function(x) points(x[1], x[2], col=knn.predict(x, data, k=10)$prediction, pch=15, cex=2))
points(data$x1, data$x2, pch=21, col='white', bg=data$y)
