library(dplyr)
library(tidyr)

data <- read.csv('http://www.creditriskanalytics.net/uploads/1/9/5/1/19511601/hmeq.csv')
data <- as_tibble(data)

data

# Column types look okay, but there are still some missing values present
sapply(data, function(x) sum(is.na(x)) / nrow(data))

# DEBTINC contains a lot of missing information, so maybe we want to encode this as such
data$DEBTINC_WASNA <- factor(as.numeric(is.na(data$DEBTINC)))

# REASON and JOB contain empty strings
table(data$REASON)
table(data$JOB)

# Let's create a separate level for these
levels(data$REASON)[1] <- 'EMPTY'
levels(data$JOB)[1] <- 'EMPTY'

library(randomForest) # Provides the helpful na.roughfix
# We could also have used MICE, missForest, or Hmisc for more advanced imputation approaches

data <- na.roughfix(data)
data

# A standard linear model (incorrect approach)
model <- lm(BAD ~ ., data=data)
hist(predict(model))

# A logistic model
model <- glm(BAD ~ ., data=data, family = binomial)
model
p <- predict(model, type = 'response')
hist(p)

table(data$BAD, p > 0.5)

library(verification)
roc.plot(data$BAD, p)

# Another approach:
library(ROCR)
ROCRpred <- prediction(p, data$BAD)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)
