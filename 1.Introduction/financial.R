# Load in packages
library('quantmod')
library('dplyr')
library('tidyr')
library('ggplot2')

# Get help using ?
?getSymbols

# Fetch the data
start <- as.Date("2017-01-01")
end <- as.Date("2017-12-31")

data <- getSymbols(c("GE", "F", "MSFT"), src="yahoo", from=start, to=end)

# Taking a look and finding the max closing price
head(F)
max(F$F.Close)

# Plot the retrieved prices
plot(F)
class(F)
plot.xts(F)
plot.zoo(F)

# Let's convert these to standard R data frames
df.F <- data.frame(date=index(F), coredata(F))
df.GE <- data.frame(date=index(GE), coredata(GE))
df.MSFT <- data.frame(date=index(MSFT), coredata(MSFT))

# Find the days where the difference between Open en Close is the largest
head(df.F)
df.F %>% mutate(diff=F.Close-F.Open) %>% arrange(desc(abs(diff))) %>% head

# Find the days where the difference between open and close > 0.30
df.F %>% mutate(diff=F.Close-F.Open) %>% filter(abs(diff) > 0.30)

# Join all data frames together, keep close only
combined <- df.F %>% 
  full_join(df.GE, by='date') %>% 
  full_join(df.MSFT, by='date') %>%
  select(date, F.Close, GE.Close, MSFT.Close)

head(combined)
table(is.na(combined))

# Plot the three stocks
plot(combined)

plot(combined$date, combined$F.Close, type='l', xlim=range(combined$date), ylim=c(0,100),
     xlab='Date', ylab='Price')
lines(combined$date, combined$GE.Close, type='l', col='red')
lines(combined$date, combined$MSFT.Close, type='l', col='blue')

# Better, with ggplot2:
combined.tidy <- combined %>% gather(stock, price, -date)
ggplot(combined.tidy, aes(date, price, color=stock)) + geom_line()

# Calculate and bin percentage changes
combined.tidy %>% 
  group_by(stock) %>% 
  mutate(percentage_change = price/lag(price) - 1) %>% 
  mutate(bracket = cut(percentage_change, breaks=seq(-1, 1, 0.01), include.lowest=T)) %T>% 
  print(head) %>%
  group_by(stock, bracket) %>% 
  summarise(n = n()) %T>% 
  print(head) %>%
  spread(stock, n)

# Calculating the volatility 
combined.tidy %>% 
  group_by(stock) %>% 
  mutate(logreturns = log(price / lag(price))) %>%
  summarise(volatility = sqrt(252*var(logreturns, na.rm=T)))

# Calculate correlations
cor(combined %>% select(-date))

