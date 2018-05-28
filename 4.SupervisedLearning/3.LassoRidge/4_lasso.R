library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(glmnet)

data <- as_tibble(read.csv('Portfolio.csv'))

# alpha is the the elasticnet mixing parameter
# alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
# glmnet applies predictor variable standardization by default
model.lasso <- glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                      y=data$PORTFOLIO, 
                      alpha=1, lambda=c(1e-6),
                      intercept=F)

model.lasso$dev.ratio         # 0.57

# Gross exposures
sum(abs(coef(model.lasso))) * 100     # 198

ggplot(data.frame(n=rownames(coef(model.lasso)), v=coef(model.lasso)[,1]), aes(x=n, y=v)) + 
  geom_bar(stat="identity") + coord_flip()

# What is the effect for a range of lambda's
lambdas <- 10 ** seq(-5, 0, 0.005)
model.lasso <- glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                      y=data$PORTFOLIO, 
                      alpha=1, lambda=lambdas,
                      intercept=F)
plot(model.lasso, "lambda", label=TRUE)

# Number of folds default = 10
cv_fit <- cv.glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                    y=data$PORTFOLIO, 
                    alpha=1, lambda=lambdas,
                    intercept=F)
plot(cv_fit)

cv_fit$lambda.min
