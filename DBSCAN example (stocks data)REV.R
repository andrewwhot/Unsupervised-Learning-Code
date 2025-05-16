# DBSCAN Stocks Example #

# Load necessary libraries
library(dbscan)
library(ggplot2)
#
# Read in "Stock data.xlsx" and name it "stocks"
# <PROVIDE YOUR OWN CODE>
#
# Plot GROWTH rate vs PROFIT rate
ggplot(stocks, aes(x=PROFIT, y=GROWTH)) +
  geom_point(color="blue")
#
# DBSCAN setup
# db <- dbscan(stocks[,3:4], eps = eps, minPts = minPts)
# Two critical parameters: eps, minPts
# eps defines the neighborhood in which DBSCAN looks for concentrations of data.
# eps is the (fixed) radius of those neighborhoods
# minPts is the minimum number of points that it takes to make a concentration
# Start with a random point in the dataset.
# Count all data within eps of that point.
# If that count exceeds minPts, call the point "core"
# and start it as a seed for a cluster.
# Then expand the cluster by recursively adding points that are
# "density reachable" from the seed. 
# A point is density reachable if the point is within eps of the seed.
# If a reachable point is also core, then add all points that are 
# reachable from it. 
# When this process of chaining together reachable points stops, then 
# repeat for the next unclassified point.
# A border point is within eps of a core point but is not core itself
# b/c it has fewer than minPts within eps of itself.
# A noise point is neither a core point nor a border point.
#
# Use 2 analysis variables (PROFIT and GROWTH) without standardizing
# Compute k-distance graph
k <- 4 # Try it!
kNNdist <- kNNdistplot(stocks[,3:4], k = k)
# Multiple potential elbows
abline(h = 3.5, col = "red", lty = 2)
#
# Apply DBSCAN with tuned parameters
eps <- 3.5
minPts <- 4
db <- dbscan(stocks[,3:4], eps = eps, minPts = minPts)
table(db$cluster)
db$cluster

# Test sensitivity to k
# Compute k-distance graph
k <- 3 # Try it!
kNNdist <- kNNdistplot(stocks[,3:4], k = k)
# Multiple potential elbows
abline(h = 3.2, col = "red", lty = 2)
#
# Apply DBSCAN with tuned parameters
eps <- 3.2
minPts <- 3
db <- dbscan(stocks[,3:4], eps = eps, minPts = minPts)
table(db$cluster)
db$cluster
# Not much difference - use second set of parameters

# Plot cluster results
ggplot(data.frame(stocks[,3:4]), aes(x = PROFIT, y =GROWTH)) +
  geom_point(aes(color = factor(db$cluster))) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering", color = "Cluster")
# Noise points are in cluster 0, but cannot tell where core and border points are.

# Determine core, boundary, and noise points
# Simultaneously keeping cluster assignments
# Create "helper" R object with the cluster assignments
x <- data.frame(stocks[,3:4], db$cluster)

# Determine core points by subsetting helper object
core_points <- x[db$cluster > 0 & sapply(1:nrow(x), 
  function(i) sum(db$cluster[dbscan::frNN(x, eps)$id[[i]]] == db$cluster[i]) >= minPts), ]
df_core <- data.frame(core_points, type="core")

# Determine border points
border_points <- x[db$cluster > 0 & !sapply(1:nrow(x), function(i) sum(db$cluster[dbscan::frNN(x, eps)$id[[i]]] == db$cluster[i]) >= minPts), ]
df_border <- data.frame(border_points, type = "border")

# Determine noise points
noise_points <- x[db$cluster == 0, ]
df_noise <- data.frame(noise_points, type = "noise")

# Put the 3 types together
all_points <- rbind(df_core,df_border,df_noise)

# Show point type and cluster number on same plot
ggplot(all_points, aes(x = PROFIT, y = GROWTH, color = factor(db.cluster))) +
  geom_point(aes(shape = type), size = 2) +
  scale_shape_manual(values = c(core = 16, border = 17, noise = 18)) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering", color = "Cluster", shape = "Point Type")
# The green cluster 1 in the lower left and the blue cluster 2 
# in the upper right are now clearly visible as separate clusters.
# Moreover, the types of the 19 points are also geographically clear as well:
# Cluster 1 has 7 core points in its interior (green circles) 
# and 1 boundary point (green triangle) on its perimeter. 
# Cluster 2 has 1 core point in its interior (blue circle)
# and 4 boundary points (blue triangles) on its the perimeter. 
# There are 6 noise points (red diamonds) not really in either identified
# cluster - although a case could be made for putting the 2 red diamonds
# in the lower left into cluster 1.