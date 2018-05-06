data <- read.csv('Cluster.csv')

head(data)

head(data[,2:3])

# "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
d <- dist(data[,2:3], method="euclidean")

# "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid"
cl <- hclust(d, method="average")

plot(cl)

cl_cut <- cutree(cl, 2)

plot(data$S, data$Spread, col=cl_cut)

