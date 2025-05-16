#############################################################
## t-SNE demonstration                                     ##
##   Austin apartment dataset                              ##
#############################################################
# Install and load the Rtsne package
install.packages("Rtsne")
library(Rtsne)

# Load example data: "AustinApartmentRent.xls" into "apts"

# Standardize all data, including Rent
apts.s <- data.frame(scale(apts))
apts.s <- scale(apts)

# Run t-SNE
# Most important parameter: perplexity
# Low values cater to local structure
# High values cater to global structure
# Another useful parameter is "learning-rate = "
tsne_result_low <-  Rtsne(apts.s, dims = 2, perplexity = 5,  verbose = TRUE, max_iter = 500)
tsne_result_high <- Rtsne(apts.s, dims = 2, perplexity = 10, verbose = TRUE, max_iter = 500)

# Plot the results
plot(tsne_result_low$Y,  col = apts$Bathrooms, pch = 19, main = "t-SNE of Austin apartments")
plot(tsne_result_high$Y, col = apts$Bathrooms, pch = 19, main = "t-SNE of Austin apartments")

#######################
# 3D portrayal
# Install the necessary packages if you haven't already
install.packages("rgl")
# install.packages("MASS")

# Load the packages
library(rgl)
# library(MASS)

# Run t-SNE for 3D portrayal
tsne_result_low <-  Rtsne(apts.s, dims = 3, perplexity = 5,  verbose = TRUE, max_iter = 500)
tsne_result_high <- Rtsne(apts.s, dims = 3, perplexity = 10, verbose = TRUE, max_iter = 500)

# Open a new RGL window
open3d()

# Plot the results
# It is useful to color by suspected clusters
plot3d(tsne_result_high$Y[,1], tsne_result_high$Y[,2], 
   tsne_result_high$Y[,3], col = apts$Bathrooms,
   type = "s", size = 1, main = "3D t-SNE of Austin apartments")

# Open a new RGL window
open3d()

# Plot the results
# It is useful to color by suspected clusters
plot3d(tsne_result_low$Y[,1], tsne_result_high$Y[,2], 
       tsne_result_low$Y[,3], col = apts$Bathrooms,
       type = "s", size = 1, main = "3D t-SNE of Austin apartments")



############################################
# Remove duplicate rows if necessary
data_unique <- unique(data_matrix)
##################
