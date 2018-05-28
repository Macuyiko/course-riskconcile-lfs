library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(glmnet)

data <- as_tibble(read.csv('Portfolio.csv'))

model.ols <- lm(PORTFOLIO ~ 0 +., data %>% select(-T))

# alpha is the the elasticnet mixing parameter
# alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
# glmnet applies predictor variable standardization by default
model.ridge <- glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                      y=data$PORTFOLIO, 
                      alpha=0, lambda=c(0.005),
                      intercept=F)

summary(model.ols)$r.squared  # 0.57
model.ridge$dev.ratio         # 0.52

# Gross exposures
sum(abs(coef(model.ols))) * 100       # 203
sum(abs(coef(model.ridge))) * 100     # 96

ggplot(data.frame(n=names(coef(model.ols)), v=coef(model.ols)), aes(x=n, y=v)) + 
  geom_bar(stat="identity") + coord_flip()

ggplot(data.frame(n=rownames(coef(model.ridge)), v=coef(model.ridge)[,1]), aes(x=n, y=v)) + 
  geom_bar(stat="identity") + coord_flip()

# What is the effect for a range of lambda's
lambdas <- 10 ** seq(-5, 0, 0.005)
model.ridge <- glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                      y=data$PORTFOLIO, 
                      alpha=0, lambda=lambdas,
                      intercept=F)
plot(model.ridge, "lambda", label=TRUE)

# Number of folds default = 10
cv_fit <- cv.glmnet(x=data %>% select(-T, -PORTFOLIO) %>% as.matrix, 
                    y=data$PORTFOLIO, 
                    alpha=0, lambda=lambdas,
                    intercept=F)
plot(cv_fit)

cv_fit$lambda.min
