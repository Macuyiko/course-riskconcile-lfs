library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)

data <- as_tibble(read.csv('Portfolio.csv'))

model <- lm(PORTFOLIO ~ 0 + ., data %>% select(-T))

summary(model)

# The futures-hedges represent a gross exposure equal to
sum(abs(coef(model))) * 100
# 203.343%

# Net exposure is
sum(coef(model)) * 100
# 127.7717

# Testing the model
new <- c(0.01, 0.01, 0.02, -0.05, 0.0, 0.01, 0.07)
predict(model, new %>% t %>% as.data.frame %>% set_colnames(names(coef(model))))
# 0.04439101 

# Taking a look at the coefficients
ggplot(data.frame(n=names(coef(model)), v=coef(model)), aes(x=n, y=v)) + 
  geom_bar(stat="identity") + coord_flip()
