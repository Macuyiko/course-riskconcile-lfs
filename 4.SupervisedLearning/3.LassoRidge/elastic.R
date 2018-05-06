library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(glmnet)

data <- as_tibble(read.csv('Portfolio.csv'))

lambdas <- 10 ** seq(-5, 0, 0.005)
model <- glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                      y=data$PORTFOLIO, 
                      alpha=0.5, lambda=lambdas,
                      intercept=F)
plot(model, "lambda", label=TRUE)

# Number of folds default = 10
cv_fit <- cv.glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                    y=data$PORTFOLIO, 
                    alpha=1, lambda=lambdas,
                    intercept=F)
plot(cv_fit)

cv_fit$lambda.min
