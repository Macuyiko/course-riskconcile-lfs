library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)
library(lubridate)
library(tidygraph)
library(ggraph)

data <- read_xlsx('Random numbers Correct.xlsx')
character_vars <- lapply(data, class) == "character"
data[, character_vars] <- lapply(data[, character_vars], as.factor)
data

data %<>% select(-contains('Random'), -X__1)
data

# Format time and data correctly
data$DateTime <- ymd(data$Date) + seconds(
  hour(ymd_hms(data$Time))*60**2 + 
    minute(ymd_hms(data$Time))*60 + 
    second(ymd_hms(data$Time)))
data$DateTime <- as.POSIXct(data$DateTime)
data %<>% select(-Date, -Time)

# Make some plots...
data %>% ggplot(aes(x=DateTime, y=Price, color=Instrument_ID)) + 
  geom_point(alpha=0.1) + 
  stat_smooth()
data %>% ggplot(aes(x=DateTime, y=Price*Quantity, color=Reporting_Dealer_ID)) + 
  geom_point(alpha=0.1) + 
  stat_smooth(alpha = 0.1)
# Looks like random data...

# Preparing our data a bit further to make graphs
data %<>% select(Reporting_Dealer_ID, Counterparty_Dealer_ID, 
                 Instrument_ID, Price, Quantity, Buy_Sell, Cleared, DateTime) %>%
  mutate(Pair=paste(Reporting_Dealer_ID, Counterparty_Dealer_ID)) %>%
  mutate(PQ=Price*Quantity)

# Making a first simple graph
data %>% head(30) %>% as_tbl_graph(directed=TRUE) %>%
  ggraph() + 
  geom_edge_link(colour='darkgrey', alpha=0.8, width=2, arrow=arrow(length=unit(5, 'mm'), type='closed')) + 
  geom_node_point(size=8, colour = 'steelblue') + 
  geom_node_text(aes(label=name)) +
  theme_graph(base_family="sans")

# Expanding this a bit
data %>% head(30) %>% as_tbl_graph(directed=TRUE) %>%
  mutate(centrality=centrality_pagerank(weights=PQ)) %>%
  ggraph() + 
  geom_edge_link(aes(width=Price*Quantity, color=Buy_Sell), alpha=0.2, arrow=arrow(length=unit(5, 'mm'), type='closed')) + 
  geom_node_point(aes(size=centrality), colour='steelblue') + 
  geom_node_text(aes(label=name), repel=TRUE) +
  theme_graph(base_family="sans")

# Now with all data, grouped first to gather all transactions
data %>% group_by(Reporting_Dealer_ID, Counterparty_Dealer_ID) %>%
  summarise(PQ_Total=sum(PQ)) %>% as_tbl_graph(directed=TRUE) %T>% print() %>%
  mutate(centrality=centrality_pagerank(weights=PQ_Total)) %>%
  ggraph() + 
  geom_edge_link(aes(width=PQ_Total), colour='cadetblue2', alpha=0.5, arrow=arrow(length=unit(5, 'mm'), type='closed')) + 
  geom_node_point(aes(size=centrality), colour='steelblue') + 
  geom_node_text(aes(label=name), repel=TRUE) +
  theme_graph(base_family="sans")

# Let's make some predictions
model.local <- glm(Cleared ~ ., data %>% select(-DateTime, -Pair), family='binomial')
model.local
p <- predict(model.local, type = 'response')
table(data$Cleared, p > 0.3)

library(ROCR)
ROCRpred <- prediction(p, data$Cleared)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)
# That doesn't look good -- though to be expected with this random data

# Relationship through time?
data$DateCuts <- cut(data$DateTime, breaks="1 day", labels=FALSE)
table(data$DateCuts, data$Cleared) %>% as.data.frame %>%
  ggplot(aes(group=Var2, color=Var2, y=Freq, x=Var1)) + geom_line()
data %<>% select(-DateCuts)

# Add lagged variables based on network information
data.prepped <- data %>% mutate(DateTimeInt=as.numeric(data$DateTime)) %>%
  rowwise %>%
  mutate(PQ=Price*Quantity) %>%
  mutate(Traded_So_Far=sum(.[.$Reporting_Dealer_ID == Reporting_Dealer_ID & .$DateTimeInt < DateTimeInt,]$PQ)) %>%
  mutate(Reporting_Dealer_ID_Cum=nrow(.[.$Reporting_Dealer_ID == Reporting_Dealer_ID & .$DateTimeInt < DateTimeInt,])) %>%
  mutate(Counterparty_Dealer_ID_Cum=nrow(.[.$Counterparty_Dealer_ID == Counterparty_Dealer_ID & .$DateTimeInt < DateTimeInt,])) %>%
  mutate(Reporting_Cleared_So_Far=sum(.[.$Reporting_Dealer_ID == Reporting_Dealer_ID & .$DateTimeInt < DateTimeInt,]$Cleared == 1)) %>%
  mutate(Reporting_NCleared_So_Far=sum(.[.$Reporting_Dealer_ID == Reporting_Dealer_ID & .$DateTimeInt < DateTimeInt,]$Cleared == 0)) %>%
  mutate(Counterparty_Cleared_So_Far=sum(.[.$Counterparty_Dealer_ID == Counterparty_Dealer_ID & .$DateTimeInt < DateTimeInt,]$Cleared == 1)) %>%
  mutate(Counterparty_NCleared_So_Far=sum(.[.$Counterparty_Dealer_ID == Counterparty_Dealer_ID & .$DateTimeInt < DateTimeInt,]$Cleared == 0))

model.network <- glm(Cleared ~ ., data.prepped %>% select(-DateTime, -Pair, -DateCuts, -DateTimeInt), family='binomial')
model.network

p <- predict(model.local, type = 'response')
ROCRpred <- prediction(p, data$Cleared)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)

p <- predict(model.network, type = 'response')
ROCRpred <- prediction(p, data.prepped$Cleared)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, add=T, col=2)
# Hooray, we're doing a little bit better :/

# Now let's take a look at pairs of traders and see how they behave through time
data %>% 
  ggplot(aes(x=DateTime, y=PQ, color=Pair)) + 
  geom_point(alpha=0.01, show.legend=F) + 
  stat_smooth(alpha = 0.01, show.legend=F)

# Let's make rolling windows to see how each pair of traders move relative to their peer group
rolling <- data %>% 
  rowwise() %>%
  mutate(AvgPQPeers = mean(.[.$Pair != Pair & .$DateTime > DateTime - days(3) & .$DateTime < DateTime,]$PQ)) %>%
  mutate(VarPQPeers = var(.[.$Pair != Pair & .$DateTime > DateTime - days(3) & .$DateTime < DateTime,]$PQ)) %>%
  mutate(AvgPQMine  = mean(.[.$Pair == Pair & .$DateTime > DateTime - days(3) & .$DateTime < DateTime,]$PQ)) %>%
  mutate(AnomScore  = (AvgPQMine - AvgPQPeers) / sqrt(VarPQPeers)) %>%
  filter(!is.na(AvgPQPeers), !is.na(AvgPQMine))

rolling %>% arrange(desc(AnomScore)) %>% View

data %>% 
  ggplot(aes(x=DateTime, y=PQ, group=Pair)) + 
  geom_line(alpha=0.04, show.legend=F) + 
  #geom_smooth(alpha=0.01, show.legend=F) + 
  geom_line(data=data %>% filter(Pair=='Dealer4 Dealer1'), aes(x=DateTime, y=PQ, group=Pair), 
            alpha = 0.6, show.legend=F, color='blue') +
  geom_line(data=rolling, aes(x=DateTime, y=AvgPQMine, group=Pair), 
            alpha = 0.01, show.legend=F, color='black') +
  geom_line(data=rolling %>% filter(Pair=='Dealer4 Dealer1'), aes(x=DateTime, y=AvgPQMine, group=Pair), 
            alpha = 0.8, show.legend=F, color='blue', size=1.5) +
  geom_point(data=rolling %>% filter(Pair=='Dealer4 Dealer1', abs(AnomScore) >= 1.5), 
             aes(x=DateTime, y=AvgPQMine, group=Pair), size=2, color='red', show.legend=F)

# More references:
# - Bolton and Hand: peer group (above) and break point analysis
# - ecp package (break point)
# - AnomalyDetection package

library(ecp)
test <- matrix(c(rnorm(100, 0, 3), rnorm(100, 2, 1), rnorm(100, 0, 3)), ncol=1)
output <- e.divisive(test, R = 499, alpha = 1)
ts.plot(test, ylab = "Value")
abline(v = output$estimates, col = "red", lty = 2)

output <- e.divisive(as.matrix(data %>% filter(Pair=='Dealer4 Dealer1') %>% .$PQ), 
                      R = 499, alpha = 1, min.size=3, sig.lvl=0.99)
ts.plot(as.matrix(data %>% filter(Pair=='Dealer4 Dealer1') %>% .$PQ, ylab = "Value"))
abline(v = output$estimates, col = "red", lty = 2)

devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
AnomalyDetectionVec(test[,1], period=10, plot=T, direction='both')
AnomalyDetectionVec(data %>% filter(Pair=='Dealer4 Dealer1') %>% mutate(PQ = PQ**2) %>% .$PQ, 
                    period=3, plot=T, direction='both',
                    alpha=0.5)



# Visualize how the network changes through time
library(animation)

data$DateCuts <- cut(data$DateTime, breaks="1 day", labels=FALSE)
graph <- as_tbl_graph(data %>% select(-DateTime) %>% head(30), directed=TRUE)
positions <- create_layout(graph=graph, layout = 'nicely')
maxpq <- max(data$Price*data$Quantity)

saveGIF({
  for(i in unique(data$DateCuts)){
    graph <- as_tbl_graph(data %>% select(-DateTime) %>% filter(DateCuts==i), directed=TRUE)
    # Recalculate positions to make sure ordering remains
    newpositions <- create_layout(graph=graph, layout = 'nicely')
    newpositions <- positions[match(newpositions$name, positions$name),]
    gr <- ggraph(graph, layout='manual', node.positions=newpositions) + 
      geom_edge_link(aes(width=Price*Quantity/maxpq, color=Buy_Sell), alpha=0.2, arrow=arrow(length=unit(5, 'mm'), type='closed')) + 
      geom_node_point(size=8, color='steelblue') + 
      geom_node_text(aes(label=name), repel=F) +
      theme_graph(base_family="sans")
    print(gr)
  }
}, interval = 0.5, ani.width = 550, ani.height = 350)


