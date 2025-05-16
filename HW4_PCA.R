## Homework 4 RScript ##

library(tidyverse)
library(mclust)
library(ggplot2)

# Used the wizard to load the mall dataset in as "mall"

mall.s <- scale(mall[2:5])
view(mall.s)

PCA.mall <- prcomp(mall.s)
names(PCA.mall)

summary(PCA.mall)
PCA.mall$sdev^2
PCA.mall$rotation

view(PCA.mall$x)
sum(PCA.mall$x[1,]^2)
sum(PCA.mall$x[1,] * PCA.mall$x[2,])

mall.s[1,]
sum(mall.s[1,]^2)
sum(mall.s[1,] * mall.s[2,])


inner_product <- sum(PCA.mall$x[[1]] * PCA.mall$x[[2]])
print(inner_product)

inner_product2 <- sum(mall.s[[1]] * mall.s[[2]])
print(inner_product2)
loadings <- PCA.mall$rotation

explained_variance <- PCA.mall$sdev^2 / sum(PCA.mall$sdev^2)
cumulative_variance <- cumsum(explained_variance)

PCA.mall$sdev
print(explained_variance)
print(cumulative_variance)

most_influential <- order(abs(loadings[, 1]), decreasing = TRUE)
top_variables <- rownames(loadings)[most_influential]

biplot(PCA.mall)

top_variables


## Part B ##

view(mag.s)
mag.s <- scale(magazine[3:18])
PCA.mag <- prcomp(magazine[3:18], scale=TRUE)

PCA.mag$sdev^2
summary(PCA.mag)


OLS_mag <- lm(magazine$Buy ~ mag.s)
summary(OLS_mag)

PCReg_mag <- lm(magazine$Buy ~ PCA.mag$x)
summary(PCReg_mag)

summary_OLS_mag <- summary(OLS_mag)
summary_OLS_mag
r_squared_full <- summary_OLS_mag$r.squared

r_squared_full

# Install and load the rsq package if you don't have it
install.packages("rsq")
library(rsq)

# Calculate the partial R-squared values for each predictor
partial_rsq <- rsq(OLS_mag)

# Print partial R-squared values for each predictor
print(partial_rsq)
