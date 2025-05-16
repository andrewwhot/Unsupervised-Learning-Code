##########################
## One-Class SVM demo   ##
##########################
# Load the necessary library
# install.packages("e1071") # if not already installed
library(e1071)

# Generate some example data with outliers
set.seed(123)
data <- matrix(rnorm(100 * 2), ncol = 2)
data <- rbind(data, matrix(rnorm(20 * 2, mean = 3), ncol = 2))

# Fit the One-Class SVM model
model_radial <- svm(data, type = 'one-classification', 
                nu = 0.1, kernel = 'radial')

# Predict anomalies
pred_radial <- predict(model_radial, data)

# Plot the results
plot(data, col = ifelse(pred_radial, 'blue', 'red'), pch = 19)

##############################
# Try different kernels
model_linear  <- svm(data, type = 'one-classification', 
                    nu = 0.1, kernel = 'linear')
model_poly    <- svm(data, type = 'one-classification', 
                    nu = 0.1, kernel = 'polynomial', degree=3)

# Predict anomalies
pred_linear <- predict(model_linear, data)
pred_poly <- predict(model_poly, data)

# Plot the results
plot(data, col = ifelse(pred_linear, 'blue', 'red'), pch = 19)
plot(data, col = ifelse(pred_poly, 'blue', 'red'), pch = 19)
###############################

# Try different nu - 20% outliers
#Generate some example data
set.seed(123)
data <- matrix(rnorm(80 * 2), ncol = 2)
data <- rbind(data, matrix(rnorm(20 * 2, mean = 3), ncol = 2))  # Add some anomalies

# Fit the One-Class SVM model with nu=0.2
model0.2 <- svm(data, type = 'one-classification', 
          nu = 0.2, kernel = 'radial')

# Predict anomalies
pred0.2 <- predict(model0.2, data)

# Plot the results
plot(data, col = ifelse(pred0.2, 'blue', 'red'), pch = 19, 
     main = 'One-Class SVM Anomaly Detection')
legend('topright', legend = c('Normal', 'Anomaly'), col = c('blue', 'red'), pch = 19)

##########################################
######################################
# One-Class SVM for high-dimensional data
################################
# Load the necessary libraries
library(e1071)
# library(caret)
library(ggplot2)

# Generate some example data
set.seed(123)
normal_data <- matrix(rnorm(900 * 50), ncol = 50)  # 900 normal samples
anomaly_data <- matrix(rnorm(100 * 50, mean = 5), ncol = 50)  # 100 anomalies with a different mean
data_big <- rbind(normal_data, anomaly_data)

# Standardize the data
data_big.s <- scale(data_big)

# Fit the One-Class SVM model
model_big.s <- svm(data_big.s, type = 'one-classification', 
                   nu = 0.1, kernel = 'radial')

# Predict anomalies
pred_big.s <- predict(model_big.s, data_big.s)

# Visualize the results using PCA for dimensionality reduction
pca_big.s <- prcomp(data_big.s)
pca_data_big.s <- data.frame(pca_big.s$x[, 1:2], Anomaly = pred_big.s)

# Plot the results
ggplot(pca_data_big.s, aes(x = PC1, y = PC2, color = Anomaly)) +
  geom_point() +
  labs(title = 'One-Class SVM Anomaly Detection (PCA Visualization)', color = 'Anomaly') +
  scale_color_manual(values = c('blue', 'red'))

