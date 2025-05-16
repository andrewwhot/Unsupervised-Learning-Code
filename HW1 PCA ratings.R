## HW1 PRINCIPAL COMPONENT ANALYSIS   ##
## Data: job_ratings.xlsx             ##
## ratings of requirements of 67 jobs ##

# Install and load packages for PCA
install.packages("stats")  # The 'stats' package is usually pre-installed
library(stats)
library(ggplot2) # For graphics

library(readxl)
ratings <- read_excel("<YOUR PATH HERE>/job_ratings.xlsx")

# Remove job code and salary variables not used in the analysis 
ratings1 <- ratings1[,2:4]

# Standardize the data to treat all variables equally
ratings_scaled <- scale(ratings1)
ratings_scaled <- as.data.frame(ratings_scaled)
View(ratings_scaled)

sd(ratings_scaled$knowhow)
# Perform PCA
pca_result <- prcomp(ratings_scaled, center = TRUE, scale. = TRUE)
    
# View the results
summary(pca_result)         # Summary of PCA results
print(pca_result$rotation)  # Principal components
print(pca_result$x)         # Transformed data (scores)

# Visualize the PCA dimensions
pca_data <- as.data.frame(pca_result$x)
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  labs(title = "PCA Plot", x = "Principal Component 1", y = "Principal Component 2")

# Two regressions
lm1 <- lm(salary ~ knowhow + problem_solving + accountability, data=ratings)
summary(lm1)
pca_data <- cbind(pca_data, ratings$salary)
lm2 <- lm(ratings$salary ~ PC1 + PC2 + PC3, data=pca_data)
summary(lm2)
