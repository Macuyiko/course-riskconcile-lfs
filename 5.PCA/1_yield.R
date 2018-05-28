library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)
library(lubridate)

data <- read.csv('Yield.csv') %>% as_tibble
data$T <- ymd(data$T)

data

data %>% gather(Swap, Rate, -T) %>% 
  ggplot(aes(x=T, y=Rate, color=Swap)) + geom_line()

# Calculate the change in interest rates (daily)
daily <- data %>% mutate_at(-T, function(x) x - lag(x)) %>% filter(!is.na(X1Y))
daily

# Center the data
daily %<>% mutate_at(-T, function(x) x - mean(x))

# Covariance matrix
m.cov <- cov(daily %>% select(-T))
eig <- eigen(as.matrix(m.cov))
eig

idx = sort(eig$values, index.return=T, decreasing=T)$ix
eig.l <- eig$values[idx]
eig.v <- eig$vectors[,idx] # Vectors column-wise
plot(1:length(eig.l), eig.l, type='b', xlab='Nbr', col='blue',
     main='Scree plot')
plot(1:length(eig.l), 100*cumsum(eig.l)/sum(eig.l), type='b', xlab='Nbr', col='blue',
     main='Variance explained')

ggplot() + geom_bar(aes(x=1:length(eig.l), y=eig.v[,1]), stat='identity') +
  xlab('Tenor') + ylab('') + ggtitle('Loading first eigenvector')

ggplot() + geom_bar(aes(x=1:length(eig.l), y=eig.v[,2]), stat='identity') +
  xlab('Tenor') + ylab('') + ggtitle('Loading second eigenvector')

ggplot() + geom_bar(aes(x=1:length(eig.l), y=eig.v[,3]), stat='identity') +
  xlab('Tenor') + ylab('') + ggtitle('Loading third eigenvector')

plot(eig.v[,1], eig.v[,2],
     xlab='First component', ylab='Second component', pch=16)

# PCA in R
ir.pca <- prcomp(daily %>% select(-T), center=F, scale.=F) 
# Same result!
plot(ir.pca$rotation[,1], ir.pca$rotation[,2],
     xlab='First component', ylab='Second component', pch=16)

