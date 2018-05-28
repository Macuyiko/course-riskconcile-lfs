library(caret)
library(readxl)
library(ggplot2)

data <- read_excel('WB.xls')

#createDataPartition(y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y)))
#createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
#createTimeSlices(y, initialWindow, horizon = 1, fixedWindow = TRUE, skip = 0)
#groupKFold(group, k = length(unique(group)))

folds <- createFolds(1:nrow(data), k=4)

folds
#$Fold1
#[1]  3  5  6 16 20 21 28 31 32 40 42 48
#$Fold2
#[1]  1  2  8 14 17 22 26 35 37 39 41 50
#$Fold3
#[1] 10 11 12 13 18 19 23 27 34 36 44 45 49
#$Fold4
#[1]  4  7  9 15 24 25 29 30 33 38 43 46 47

par(mfrow=c(2,2))
for (name in names(folds)) {
  plot(data[folds[[name]],]$P, data[folds[[name]],]$D)
}
par(mfrow=c(1,1))

# A better method with ggplot2

data$fold <- apply(
  sapply(1:nrow(data), 
         function(x) sapply(folds, function(f) x %in% f)), 
  2, function(x) which(x))

ggplot(data, aes(P, D, color=factor(fold))) + geom_point()
