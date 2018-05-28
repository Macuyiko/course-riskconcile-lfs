library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)

data <- as_tibble(read.csv('Portfolio.csv'))

vars <- setdiff(colnames(data), c('T', 'PORTFOLIO'))
all_combinations <- sapply(1:length(vars) , function(m) combn(vars, m))

all_models <- sapply(1:length(vars), function(l) {
  cat(paste('Checking subsets with length:', l, '\n'))
  models <- apply(all_combinations[[l]], 2, function(x) {
    model <- lm(PORTFOLIO ~ 0 + ., data %>% select(PORTFOLIO, one_of(x)))
  })
})

best_per_subset <- sapply(1:length(vars), function(l)
  min(sapply(all_models[[l]], function(model) summary(model)$sigma)))

plot(1:length(vars), best_per_subset * 1000, type='l', 
     xlab='Nr. of vars considered', 
     ylab='Best RSS * 1000')
points(1:length(vars), best_per_subset * 1000, pch=16)
sapply(1:length(vars), function(l) {
  sigmas <- sapply(all_models[[l]], function(model) summary(model)$sigma)
  points(rep(l, length(sigmas)), sigmas * 1000)
  sigmas
})





# Stepwise selection using step:
base_model <- lm(PORTFOLIO ~ 0 + ., data %>% select(-T))
slm <- step(base_model, direction="both")
summary(slm)
