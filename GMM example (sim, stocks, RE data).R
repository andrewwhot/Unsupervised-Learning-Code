######################################################
## DEMO FOR GMM (Gaussian Mixture Models) CLUSTERING##
##  using simulated data, stocks data,              ##
##  random100 (including Zone and defs).xlsx        ##
######################################################
# Load necessary libraries
install.packages("mclust")
library(mclust)
library(ggplot2)

## Simulated data
# Generate sample data
set.seed(1234)
n <- 500
x <- cbind(
  x = c(rnorm(n/2, mean = 0, sd = 1), rnorm(n/2, mean = 5, sd = 1)),
  y = c(rnorm(n/2, mean = 0, sd = 1), rnorm(n/2, mean = 5, sd = 1))
)

# Apply GMM
gmm <- Mclust(x)

# Plot results
ggplot(data.frame(x), aes(x = x, y = y)) +
  geom_point(aes(color = factor(gmm$classification))) +
  theme_minimal() +
  labs(title = "GMM Clustering", color = "Cluster")

# Useful interpretive parameters
summary(gmm)
gmm$loglik
gmm$parameters$pro
gmm$parameters$mean
gmm$parameters$variance

##########################################
## Stocks data 
# Read in "Stock data.xlsx" as R object "stocks"
# Apply GMM
gmm <- Mclust(stocks[,3:4])

# Plot results
ggplot(data.frame(stocks[,3:4]), aes(x = PROFIT, y = GROWTH)) +
  geom_point(aes(color = factor(gmm$classification))) +
  theme_minimal() +
  labs(title = "GMM Clustering", color = "Cluster")

#######################################
# Cluster stocks data using all 3 variables
# Apply GMM
gmm <- Mclust(stocks[,2:4])

# Plot results
ggplot(data.frame(stocks[,2:4]), aes(x = PROFIT, y = GROWTH)) +
  geom_point(aes(color = factor(gmm$classification))) +
  theme_minimal() +
  labs(title = "GMM Clustering", color = "Cluster")

# Too many clusters
# Print posterior probabilities of cluster membership
print(gmm$z)
# Check BIC
plot(gmm, what = "BIC")
print(gmm$BIC, what = "BIC")
# Optimal model selected was EEV = Equal volume, equal shape, variable orientation
# Override auto-selection
gmm <- Mclust(stocks[,2:4], G = 2:5)

# Plot results
ggplot(data.frame(stocks[,2:4]), aes(x = PROFIT, y = GROWTH)) +
  geom_point(aes(color = factor(gmm$classification))) +
  theme_minimal() +
  labs(title = "GMM Clustering", color = "Cluster")

########################################
## Real estate data
# Read in "random100 (including Zone and defs).xlsx" as "homes"
# Apply GMM to non-location quantitative variables
gmm <- Mclust(homes[,4:13])
table(gmm$classification)
# Print posterior probabilities of cluster membership
print(gmm$z)
# Check BIC
plot(gmm, what = "BIC")
print(gmm$BIC, what = "BIC")
# Optimal model selected was EEE = Equal volume, equal shape, equal orientation

# Plot results
ggplot(data.frame(homes[,4:13]), aes(x = area, y = price)) +
  geom_point(aes(color = factor(gmm$classification))) +
  theme_minimal() +
  labs(title = "GMM Clustering", color = "Cluster")

# Useful interpretive parameters
summary(gmm)
gmm$loglik
gmm$parameters$pro
gmm$parameters$mean
gmm$parameters$variance
