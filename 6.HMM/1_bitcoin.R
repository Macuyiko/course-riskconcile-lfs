library(dplyr)
library(ggplot2)
library(readxl)

data <- read_excel('Bitcoindata.xls')

data %<>%
  mutate(RET = Close / lag(Close) - 1) %>%
  mutate(LOGRET = log(1 + RET)) %>% 
  filter(!is.na(RET))

volatility <- sd(data$LOGRET) * sqrt(250) * 100

data %>% ggplot(aes(x=Date, y=Close)) + geom_line() +
  ggtitle(paste('Volatility of Bitcoin = ', volatility))

qqn <- qqnorm(data$LOGRET); qqline(data$LOGRET)
cor(qqn$x, qqn$y)

library(NormalLaplace)

qqn <- qqnl(data$LOGRET, line=F)
cor(qqn$x, qqn$y)

# Kolmogorov-Smirnov test
# -> rejected
ks.test(data$LOGRET, 'pnorm')

# Studying the bitcoin returns as the combination of three gaussians
# Each gaussian represents a regime

library(mclust)
library(mixtools)
library(depmixS4)

# mclust
model.mclust <- Mclust(data$LOGRET, G=3, 'V')
summary(model.mclust)
plot(model.mclust, what="density")
for (i in 1:3){
  tmp<-model.mclust$classification==i  
  lines(density(model.mclust$data[tmp],adjust=2),col=i+1,lwd=2)  
}

data$C <- factor(model.mclust$classification)
data %>% ggplot(aes(x=Date, y=LOGRET, color=C, group=1)) + geom_point(size=2)

# mixtools
model.gmm <- normalmixEM(data$LOGRET, k=3)
plot(model.gmm, density=T)
summary(model.gmm)

data$C <- factor(apply(model.gmm$posterior, 1, function(x) which(x==max(x))))
data %>% ggplot(aes(x=Date, y=LOGRET, color=C, group=1)) + geom_point(size=2)

# depmixS4
hmm <- depmix(LOGRET ~ 1, family=gaussian(), nstates=3, data=data)
hmmfit <- fit(hmm, emcontrol=em.control(classification='hard'))
summary(hmmfit)

data$C <- factor(hmmfit@posterior$state)
data %>% ggplot(aes(x=Date, y=LOGRET, color=C, group=1)) + geom_point(size=2)
