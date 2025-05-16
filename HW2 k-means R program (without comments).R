##################################
## HOMEWORK 2                   ##   
## K-means program with data:   ##
##   Customers (magazines).xlsx ##
##################################
library(stats)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(readxl)
# customers <- read_excel("<YOUR FOLDER REFERENCE HERE>/Customers (magazines).xlsx")
View(customers)
customers.sub <- customers[,3:18]
customers.s <- scale(customers.sub)
set.seed(1234)
fviz_nbclust(customers.s, kmeans, method = "wss")
set.seed(1234)
fviz_nbclust(customers.s, kmeans, method = "silhouette")
set.seed(1234)
gap_stat <- clusGap(customers.s, FUN = kmeans, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
set.seed(1234)
fviz_nbclust(customers.s, kmeans, method = "silhouette", nstart=25)
set.seed(1234)
k4 <- kmeans(customers.s, centers = 4, nstart = 25)
set.seed(1234)
k5 <- kmeans(customers.s, centers = 5, nstart = 25)
set.seed(1234)
k6 <- kmeans(customers.s, centers = 6, nstart = 25)
set.seed(1234)
k7 <- kmeans(customers.s, centers = 7, nstart = 25)
set.seed(1234)
k8 <- kmeans(customers.s, centers = 8, nstart = 25)
p4 <- fviz_cluster(k4, geom = "point", data = customers.s) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point", data = customers.s) + ggtitle("k = 5")
p6 <- fviz_cluster(k6, geom = "point", data = customers.s) + ggtitle("k = 6")
p7 <- fviz_cluster(k7, geom = "point", data = customers.s) + ggtitle("k = 7")
p8 <- fviz_cluster(k8, geom = "point", data = customers.s) + ggtitle("k = 8")
grid.arrange(p4, p5, p6, p7, p8, nrow = 2)
table(k6$cluster)
aggregate(. ~ k6$cluster, data=customers.sub, FUN=mean)

print(k6)
k6$totss
k6$betweenss

## PCA INQUIRIES ##

pca_result <- prcomp(customers.s, center = TRUE, scale. = TRUE)
summary(pca_result)         # Summary of PCA results
print(pca_result$rotation)  # Principal components
print(pca_result$x)         # Transformed data (scores)

pca_data <- as.data.frame(pca_result$x)
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  labs(title = "PCA Plot", x = "Principal Component 1", y = "Principal Component 2")

p4
p6
