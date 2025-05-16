## HW1 CLUSTER ANALYSIS               ##
## Data: job_ratings.xlsx             ##
## ratings of requirements of 67 jobs ##

# Install and load hierarchical agglomerative cluster analysis package
install.packages("cluster")
library(cluster)

library(readxl)
ratings <- read_excel("<YOUR PATH HERE>/job_ratings.xlsx")
View(ratings)

# Remove job code variable not used in the analysis 
ratings <- ratings[,2:5]

# Standardize the data
ratings_scaled <- scale(ratings)
ratings_scaled <- as.data.frame(ratings_scaled)
View(ratings_scaled)
mean(ratings_scaled_clus$knowhow)
# Calculate the distance matrix
dist_matrix <- dist(ratings_scaled)

# Perform hierarchical clustering using Ward's original algorithm
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc)

# Cut the dendrogram to get clusters
clustnum <- cutree(hc, k = 5)  # Change k to the desired number of clusters

# Add cluster number to data
ratings_clus <- cbind(ratings, clustnum)
ratings_scaled_clus <- cbind(ratings_scaled, clustnum)

# Calculate the means and sds by the cluster number

# Install and load dplyr to get means by cluster number
install.packages("dplyr")
library(dplyr)

# Calculate the means and frequencies by clustnum
means_and_counts <- summarize(
  group_by(ratings_clus, clustnum),
  count = n(),
  across(where(is.numeric), mean, na.rm = TRUE)
)
# Calculate the sds by clustnum for scaled data
sds <- summarize(
  group_by(ratings_scaled_clus, clustnum),
  across(where(is.numeric), sd, na.rm = TRUE)
)
# Add frequencies to sds by clustnum
sds_and_counts <- cbind(sds,means_and_counts$count)

View(means_and_counts)
View(sds_and_counts)

# Export results to Excel for further analysis
# Install and load rio package to get export function
install.packages("rio")
library(rio)

# Export means and counts to Excel
export(means_and_counts,"D:/(Downloads/means_and_counts.xlsx")
# Export means and counts to Excel
export(sds_and_counts,"D:/(Downloads/sds_and_counts.xlsx")
