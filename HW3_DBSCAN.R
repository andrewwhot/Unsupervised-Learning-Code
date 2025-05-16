# Load necessary libraries
library(dbscan)
library(ggplot2)
library(tidyverse)

# I used the wizard to load in the homes data!
view(homes)

homes.s <- scale(homes[4:13])
view(homes.s)

# Compute k-distance graph

kNNdist <- kNNdistplot(homes.s, k = 3)
abline(h = 2.1, col = "red", lty = 2)
abline(h = 3.4, col = "red", lty = 2)
abline(h = 3.5, col = "red", lty = 2)

kNNdist2 <- kNNdistplot(homes.s, k = 4)
abline(h = 4.2, col = "red", lty = 2)
abline(h = 3.5, col = "red", lty = 2)

kNNdist3 <- kNNdistplot(homes.s, k = 5)
abline(h = 3.25, col = "red", lty = 2)

kNNdist4 <- kNNdistplot(homes.s, k = 6)
abline(h = 3.5, col = "red", lty = 2)

kNNdist5 <- kNNdistplot(homes.s, k = 7)
abline(h = 3.5, col = "red", lty = 2)

## DBSCAN ##

eps <- 3.4
minPts <- 4
db4 <- dbscan(homes.s, eps = eps, minPts = minPts)
table(db4$cluster)
db4$cluster

aggregate(. ~ db4$cluster, data=homes[4:13], FUN=mean)
aggregate(. ~ db4$cluster, data=homes[13:21], FUN=mean)


sum_of_squares <- function(x) {
  mean_x <- mean(x)
  sum((x - mean_x)^2)
}

calculate_wss <- function(data, clusters) {
  wss <- 0
  for (k in unique(clusters)) {
    cluster_data <- data[clusters == k, , drop = FALSE]
    cluster_center <- matrix(colMeans(cluster_data), nrow = nrow(cluster_data), ncol = ncol(cluster_data), byrow = TRUE)
    wss <- wss + sum((cluster_data - cluster_center)^2)
  }
  return(wss)
}

wss <- calculate_wss(homes.s, db4$cluster)
wss

view(homes.s)
length(homes.s)
sum_of_squares(homes.s)
tss = 990
rsquare <- 1 - (wss/tss)
rsquare

ggplot(homes[4:13], aes(x = area, y = price, color = factor(db4$cluster))) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering", color = "Cluster")

x <- data.frame(homes.s, db4$cluster)

# Determine core points by subsetting helper object
core_points <- x[db4$cluster > 0 & sapply(1:nrow(x), 
                                         function(i) sum(db4$cluster[dbscan::frNN(x, eps)$id[[i]]] == db4$cluster[i]) >= minPts), ]
core_points
df_core <- data.frame(core_points, type="core")
df_core
df_core$db4.cluster == 2

border_points <- x[db4$cluster > 0 & !sapply(1:nrow(x), function(i) sum(db4$cluster[dbscan::frNN(x, eps)$id[[i]]] == db4$cluster[i]) >= minPts), ]
df_border <- data.frame(border_points, type = "border")


noise_points <- x[db4$cluster == 0, ]
df_noise <- data.frame(noise_points, type = "noise")

all_points <- rbind(df_core,df_border,df_noise)

all_points

ggplot(homes.s, aes(x = area, y = price, color = factor(db4$cluster))) +
  geom_point(aes(shape = all_points$type), size = 2) +
  scale_shape_manual(values = c(core = 16, border = 17, noise = 18)) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering", color = "Cluster", shape = "Point Type")

aggregate(. ~ all_points$type, data=homes[14:21], FUN=mean)

ggplot(homes[4:13], aes(x = area, y = price, color = factor(db4$cluster))) +
  geom_point(aes(shape = all_points$type), size = 2) +
  scale_shape_manual(values = c(core = 16, border = 17, noise = 18)) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering", color = "Cluster", shape = "Point Type")

### Part 3: GMM ###

library(mclust)
library(ggplot2)

# Used the wizard to load in Random100 data
homes.s <- scale(homes[4:20])
view(homes.s)

gmm <- Mclust(homes.s, 2:7)
table(gmm$classification)
print(gmm$z)

plot(gmm, what = "BIC")
print(gmm$BIC, what = "BIC")

aggregate(. ~ gmm$classification, data=homes[4:13], FUN=mean)

summary(gmm)
gmm$loglik
gmm$parameters$pro
gmm$parameters$mean
gmm$parameters$variance

clusters <- gmm$classification
means <- gmm$parameters$mean

wss <- calculate_wss(homes.s, clusters)
print(wss)

# Total Sum of Squares
overallMean <- mean(homes.s)
TSS <- sum((homes.s - overallMean)^2)
print(TSS)

Rsquare <- 1 - (wss/TSS)
Rsquare
rsquare



