library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)
library(lubridate)

data <- read.csv('PLS_PCR_Comp.csv') %>% as_tibble
data

ggplot(data, aes(x=X1, y=X2, color=Y)) + geom_point() +
  geom_text(aes(label=Y), nudge_x=.1)

# PCR from PCA
ir.pca <- prcomp(data %>% select(-Y), center=F, scale.=F) 
eigval <- ir.pca$sd^2
eigvec.largest <- ir.pca$rotation[,'PC1']

X_Project = data[, c("X1", "X2")] %>% as.matrix() %*% eigvec.largest
CorrCoef = cor(X_Project, data$Y)
print(CorrCoef**2) # 0.459782

ggplot(data, aes(x=X1, y=X2, color=Y)) + geom_point() +
  geom_text(aes(label=Y), nudge_x=.1) +
  geom_segment(aes(x=-3*eigvec.largest[1], y=-3*eigvec.largest[2],
                   xend=3*eigvec.largest[1], yend=3*eigvec.largest[2]), linetype=2) +
  ggtitle(paste('R2 with First Princ. Comp.', CorrCoef**2))

# Project data on first component rotated
a <- 1:180*3.1415/180
c <- sapply(a, function(a) {
  X_Projected = data[, c("X1", "X2")] %>% as.matrix() %*% c(cos(a), sin(a))
  cor(X_Projected, data$Y)**2
})

plot(a, c, type='l', xlab='alpha (orientation of scorevector)', ylab='R2',
     main='Optimal score and loading')
points(a[which(c == max(c))], max(c), col='red', pch=16)

V_opt = c(cos(a[which(c == max(c))]), sin(a[which(c == max(c))]))
X_Project = data[, c("X1", "X2")] %>% as.matrix() %*% V_opt
CorrCoef = cor(X_Project, data$Y)
print(CorrCoef**2)

ggplot(data, aes(x=X1, y=X2, color=Y)) + geom_point() +
  geom_text(aes(label=Y), nudge_x=.1) +
  geom_segment(aes(x=-3*V_opt[1], y=-3*V_opt[2],
                   xend=3*V_opt[1], yend=3*V_opt[2]), linetype=2) +
  ggtitle(paste('R2 with First Princ. Comp.', CorrCoef**2))

# Partial least squares regression
library(pls)
model <- plsr(Y ~ X1 + X2, ncomp=1, data=data, validation='none')
summary(model)

V_proc = loading.weights(model)[,1]
X_Project = data[, c("X1", "X2")] %>% as.matrix() %*% V_proc
CorrCoef = cor(X_Project, data$Y)
print(CorrCoef**2)

ggplot(data, aes(x=X1, y=X2, color=Y)) + geom_point() +
  geom_text(aes(label=Y), nudge_x=.1) +
  geom_segment(aes(x=-3*V_opt[1], y=-3*V_opt[2],
                   xend=3*V_opt[1], yend=3*V_opt[2]), linetype=2) +
  ggtitle(paste('R2 with First Princ. Comp.', CorrCoef**2))

