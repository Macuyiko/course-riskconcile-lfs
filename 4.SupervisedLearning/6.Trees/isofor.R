library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)

# Both iforest and isofor are not available on CRAN yet
install.packages("devtools")
devtools::install_github("Zelazny7/isofor")
library(isofor)

data <- read_excel('TreeData.xls') %>% as_tibble
data$Y <- factor(data$Y)

model <- iForest(data, nt=100, phi=5)
data$p <- predict(model, data)

ggplot(data=data, aes(X1, X2, color=p)) + 
  geom_point(size=10) +
  scale_colour_gradient(low = "black", high = "red") +
  theme_bw()

x <- c(rnorm(1e3, 0, 0.5), rnorm(1e3*0.05, -1.5, 1))
y <- c(rnorm(1e3, 0, 0.5), rnorm(1e3*0.05,  1.5, 1))
data <- data.frame(x, y)
model <- iForest(data, nt=100)
data$p <- predict(model, data)

ggplot(data=data, aes(x, y, color=p)) + 
  geom_point(size=3) +
  scale_colour_gradient(low = "black", high = "red") +
  theme_bw()
