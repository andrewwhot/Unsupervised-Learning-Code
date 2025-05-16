library(mosaic)
library(tidyverse)

MexBank.s <- scale(MexBank[,-1])
head(MexBank.s)

dist_MexBank.s <- dist(MexBank.s, method="euclidean")

CA.MexBank.s <- hclust(dist_MexBank.s,method="ward.D2")

plot(CA.MexBank.s)

cl_MexBank.s5 = cutree(CA.MexBank.s, k=5)
cl_MexBank.s5
table(cl_MexBank.s5)

aggregate(. ~ cl_MexBank.s5, data=MexBank, FUN=mean)

calculate_wss <- function(data, clusters) {
  wss <- 0
  for (k in unique(clusters)) {
    cluster_data <- data[clusters == k, , drop = FALSE]
    cluster_center <- matrix(colMeans(cluster_data), nrow = nrow(cluster_data), ncol = ncol(cluster_data), byrow = TRUE)
    wss <- wss + sum((cluster_data - cluster_center)^2)
  }
  return(wss)
}

calculate_wss(MexBank.s, cl_MexBank.s5)



## NEXT PART ##



