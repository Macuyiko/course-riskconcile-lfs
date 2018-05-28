library(dplyr)
library(zoo)
library(ggplot2)

# Dowload and clean GBPEUR data
data <- read.csv('https://finance.google.com/finance/getprices?q=GBPEUR&p=5Y&i=86400', 
                 stringsAsFactors=F, header=F)

data.clean <- data[-(1:7),]
colnames(data.clean) <- c('TIME', 'CLOSE', 'HIGH', 'LOW', 'OPEN', 'VOLUME')
data.clean$TIME <- as.character(data.clean$TIME)
data.clean %<>% 
  mutate(
    counter=ifelse(startsWith(TIME, 'a'), 0, as.numeric(TIME)),
    base=ifelse(startsWith(TIME, 'a'), as.numeric(substring(TIME, 2)), NA)
  ) %>%
  mutate(base = na.locf(base)) %>% 
  mutate(TIME = as.POSIXct(counter * 86400 + base, origin="1970-01-01"),
         CLOSE = as.numeric(CLOSE))

data.clean %>% ggplot(aes(x=TIME, y=CLOSE)) + geom_line()

# Calculating (log) returns for the dataset

data.clean %<>% select(TIME, CLOSE) %>%
  mutate(RET = CLOSE / lag(CLOSE) - 1) %>%
  mutate(LOGRET = log(1 + RET)) %>% 
  filter(!is.na(RET))

# We opt for a hidden markov model with 3 hidden states
# Each state has a Gaussian Emission to visible states
# The visible states are the sequence of daily logreturns observed for the GBPEUR ER

library(depmixS4)

hmm <- depmix(LOGRET ~ 1, family=gaussian(), nstates=3, data=data.clean)
hmmfit <- fit(hmm, emcontrol=em.control(classification='hard'))

summary(hmmfit)

# Two states are more appropriate

hmm <- depmix(LOGRET ~ 1, family=gaussian(), nstates=2, data=data.clean)
hmmfit <- fit(hmm, emcontrol=em.control(classification='hard'))

summary(hmmfit)

data.clean$P <- factor(hmmfit@posterior$state)
data.clean %>% ggplot(aes(x=TIME, y=LOGRET, color=P, group=1)) + geom_line(size=1)
data.clean %>% ggplot(aes(x=TIME, y=CLOSE, color=P, group=1)) + geom_line(size=1)


# Other packages:
# depmixS4
# HiddenMarkov
# rarhsmm
# HMMCont
# RcppHMM
# mhsmm
# RHmm