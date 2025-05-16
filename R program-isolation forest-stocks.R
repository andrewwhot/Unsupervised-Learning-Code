################################################################
## Example of Isolation Forest and Extended Isolation Forest  ##
##   simulated data with outliers                             ##
##   and "Stock data.xlsx"                                    ##
################################################################

# Install and load the isotree package
install.packages("isotree") # if not already installed
library(isotree)

# Generate a sample dataset
# 100 pairs of standard normal data + 10 outliers
set.seed(123)
data <- data.frame(
  x = c(rnorm(100), rnorm(10, mean = 5)),
  y = c(rnorm(100), rnorm(10, mean = 5))
)

# Fit the original Isolation Forest model
model_original <- isolation.forest(data, ntrees = 100, ndim = 1)

# Fit the Extended Isolation Forest model
model_eif <- isolation.forest(data, ntrees = 100, ndim = 2)

# Predict anomaly scores
scores_original <- predict(model_original, data)
scores_eif <- predict(model_eif, data)

# Plot the results for the original Isolation Forest
plot(data$x, data$y, col = ifelse(scores_original > 0.5, 
     "red", "blue"), pch = 19,
     main = "Original Isolation Forest Anomaly Detection",
     xlab = "X", ylab = "Y")
     legend("topright", legend = c("Normal", "Anomaly"), col = c("blue", "red"), pch = 19)

# Plot the results for the Extended Isolation Forest
plot(data$x, data$y, col = ifelse(scores_eif > 0.5, "red", "blue"), pch = 19,
     main = "Extended Isolation Forest Anomaly Detection",
     xlab = "X", ylab = "Y")
# legend("topright", legend = c("Normal", "Anomaly"), col = c("blue", "red"), pch = 19)

######################################################
# Here is the SCiForest extension.
# Unlike IF and EIF, SCiForest does not use random splits. 
# Instead SCiForest uses a selection criterion to guide the choice
# of splits to try to isolate anomalies more efficiently. 
# Like EIF, SCiForest uses general hyperplanes.
# SCiForest is especially effective at finding clustered anomalies
# close to each other - which other methods often miss.
# The distinguishing parameter for SCiForest is prob_pick_avg_gain.
# Fit the SCiForest model (continuing preceding sim data example)
model_sciforest <- isolation.forest(data, ntrees = 100, 
                   ndim = 2, prob_pick_avg_gain = 0.5)
# Here, half the splits are chosen randomly,
# the other half by maximizing gain in average isolation.
# Predict anomaly scores
scores_sciforest <- predict(model_sciforest, data)

# Plot the results
plot(data$x, data$y, col = ifelse(scores_sciforest > 0.5, 
     "red", "blue"), pch = 19,
     main = "SCiForest Anomaly Detection",
     xlab = "X", ylab = "Y")
# legend("topright", legend = c("Normal", "Anomaly"), col = c("blue", "red"), pch = 19)


################################################### 
# Here is the Fair-Cut Forest extension.
# FCF is another attempt to improve splitting.
# FCF also uses hyperplanes. But it tries to avoid favoring certain
# features or data points when making splits. FCF makes more balanced
# trees than IF and downplays noise in the data.
# Fit the Fair-Cut Forest model
model_fcf <- isolation.forest(data, ntrees = 100, 
             ndim = 2, prob_pick_pooled_gain = 0.5)
# Here, half the splits are chosen randomly,
# the other half by maximizing overall gain in isolation
# pooled across all features.

# Predict anomaly scores
scores_fcf <- predict(model_fcf, data)

# Plot the results
plot(data$x, data$y, col = ifelse(scores_fcf > 0.5, 
     "red", "blue"), pch = 19,
     main = "Fair-Cut Forest Anomaly Detection",
     xlab = "X", ylab = "Y")
#legend("topright", legend = c("Normal", "Anomaly"), col = c("blue", "red"), pch = 19)
###########################################
# "Stock data.xlsx" example with IF and EIF 
# Read in "Stock data.xlsx" into R object stocks

# Fit the original Isolation Forest model
stocks_IF <- isolation.forest(stocks[,2:4], ntrees = 100, ndim = 1)

# Fit the Extended Isolation Forest model
stocks_EIF <- isolation.forest(stocks[,2:4], ntrees = 100, ndim = 2)

# Predict anomaly scores
stocks_IF_scores  <- predict(stocks_IF,  stocks[,2:4])
stocks_EIF_scores <- predict(stocks_EIF, stocks[,2:4])

# Plot the results for the original Isolation Forest
plot(stocks$P_E, stocks$PROFIT, col = ifelse(stocks_IF_scores > 0.5, 
     "red", "blue"), pch = 19,
     main = "Original Isolation Forest Anomaly Detection",
     xlab = "P_E", ylab = "PROFIT")
#legend("topright", legend = c("Normal", "Anomaly"), col = c("blue", "red"), pch = 19)

# Display anomaly scores
stocks_IF_scores

# Plot the results for the Extended Isolation Forest
plot(stocks$P_E, stocks$PROFIT, col = ifelse(stocks_EIF_scores > 0.5, 
     "red", "blue"), pch = 19,
     main = "Extended Isolation Forest Anomaly Detection",
     xlab = "P_E", ylab = "PROFIT")
#legend("topright", legend = c("Normal", "Anomaly"), col = c("blue", "red"), pch = 19)

# Display anomaly scores
stocks_EIF_scores
