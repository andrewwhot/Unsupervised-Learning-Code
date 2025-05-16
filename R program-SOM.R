###########################################
## Self-Organizing Maps (SOM)            ##
#    with kohonen package in R            #
###########################################

# Install and load the kohonen package
# install.packages("kohonen") # if not already installed
library(kohonen)

# Example with simulated data
# Generate some sample data
set.seed(123)
data <- matrix(rnorm(1000), ncol = 10)

# Scale the data
data_scaled <- scale(data)

# Define the SOM grid
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

# Train the SOM
# rlen=100 specifies 100 iterations
# alpha=c(0.05,0.01) specifies the learning rate schedule, 
#  starting at 0.05 and decreasing to 0.01
# Because of random aspects of training, can set the seed to 
# try to establish reproducibility
set.seed(123)
som_model <- som(data_scaled, grid = som_grid, rlen = 100, 
                 alpha = c(0.05, 0.01))

# Check if convergence has been achieved
# Plot the quantization error
# Quantization Error: This measures the average distance between each data
# point and its Best Matching Unit (BMU). A decreasing quantization error
# indicates that the SOM is learning and adapting to the data. 
# When this error stabilizes, it suggests that the map has sufficiently 
# learned the data structure.
plot(som_model, type = "changes", main = "Quantization Error Over Iterations")

# Access the codebook vectors correctly
# The codebook vectors are the prototype vectors associated with each
# neuron in the SOM. They represent the typical data points for the neurons.
codes <- getCodes(som_model)

# Plot the SOM
plot(som_model, type = "codes", palette.name = rainbow)

# Heatmaps
# Create a heatmap for the first variable
# Each variable has a heatmap
plot(som_model, type = "property", property = codes[,1], 
     main = "Heatmap of First Variable")

######################
# Plot heatmaps of multiple variables at once
# Set up the layout for multiple plots
par(mfrow = c(2, 2))  # Adjust the layout as needed

# Create heatmaps for the first four variables
for (i in 1:4) {
  plot(som_model, type = "property", property = codes[,i], 
       main = paste("Heatmap of Variable", i))
}

# Restore original plotting dimensions
par(mfrow=c(1,1))

#########################
# Illustrate other plotting features of kohonen
# using Fisher's iris data
data(iris)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
set.seed(123)
som_model <- som(scale(iris[, -5]), grid = som_grid)

# 1.	Component Planes:
# Each component plane shows the distribution of one variable 
# across the map. For example, if you have a dataset with variables
# like age, income, and spending score, each component plane will
# visualize one of these variables.
plot(som_model, type = "property", property = som_model$codes[[1]][,1], 
     main = "Component Plane for Sepal Length")

# 2.	U-Matrix (Unified Distance Matrix):
# The U-Matrix visualizes the distances between neighboring neurons. 
# High values (darker colors) indicate larger distances, 
# suggesting cluster boundaries.
plot(som_model, type = "dist.neighbours", main = "U-Matrix")

#3.	Hit Histogram:
# This plot shows the number of data points mapped to each neuron. 
# It helps in understanding the density and distribution of data points 
# across the map.
plot(som_model, type = "counts", main = "Hit Histogram")

#4.	Cluster Boundaries:
# If clustering is performed on the SOM, the boundaries of these 
# clusters can be overlaid on the map. This helps in visualizing 
# different groups within your data.
som_clusters <- cutree(hclust(dist(som_model$codes[[1]])), 3)
plot(som_model, type = "mapping", bgcol = rainbow(3)[som_clusters], 
     main = "Cluster Boundaries")
add.cluster.boundaries(som_model, som_clusters)

# 5.	Labels and Mapping:
# If your data is labeled, you can map these labels onto the SOM 
# to see how different classes are distributed. This is useful for 
# classification tasks.
plot(som_model, type = "mapping", labels = iris$Species, 
     col = as.integer(iris$Species), main = "Labels and Mapping")

# 6.	Neighbor Weight Distances:
# This plot shows the distances between the weight vectors of 
# neighboring neurons. It helps in identifying regions of the map 
# where the data is more homogeneous or heterogeneous.
plot(som_model, type = "quality", main = "Neighbor Weight Distances")

