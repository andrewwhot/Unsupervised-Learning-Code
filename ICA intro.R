#######################################
# Independent Component Analysis Demo #
#  fastICA Demo                       #
#  Simulation example                 #
#######################################
# Install and load the fastICA package
install.packages("fastICA")
library(fastICA)

# How does ICA work for data with a single source but 2 signals?
# Generate sample data
set.seed(123)
S <- matrix(rnorm(2000), 1000, 2)
A <- matrix(c(1, 2, 3, 4), 2, 2)
X <- S %*% t(A) # %*% is used for matrix multiplication

# Apply ICA
ica_result <- fastICA(X, n.comp = 2) # n.comp corresponds to the number of signals looking for

# Plot the results
par(mfrow = c(1, 2))
plot(ica_result$S[, 1], type = "l", main = "Independent Component 1")
plot(ica_result$S[, 2], type = "l", main = "Independent Component 2")

################################################
# Install and load the fastICA package
# Example of 4 signals and 3 sources
# Fisher's iris data

# Load example data
data(iris)
iris_matrix <- as.matrix(iris[, -5])

# Run ICA
ica_result <- fastICA(iris_matrix, n.comp = 3)

# Plot the results
pairs(ica_result$S, col = iris$Species, main = "ICA of Iris Data")
######################
# Removing duplicate rows is recommended
# Remove duplicate rows
iris_matrix_unique <- unique(iris_matrix)

# Run ICA
ica_result <- fastICA(iris_matrix_unique, n.comp = 3)

# Plot the results
pairs(ica_result$S, col = iris$Species[!duplicated(iris_matrix)], main = "ICA of Iris Data")

############################################
# Stocks data example
# Read in stocks data
stocks_ica <- fastICA(stocks[,2:4], n.comp=2)

# Plot the results
pairs(stocks_ica$S, col=factor(stocks$Industry), main = "ICA for Stocks Data")

# Suppose I suspect 4 signals (industries)?
stocks_ica4 <-fastICA(stocks[,2:4], n.comp=4)

# May use the SCA package, but ...
install.packages("SCA")
# it is not available at CRAN
