# Principal Components Analysis
# Basic R commands
# Illustrated with "Stock data.xlsx"
# entering raw data and extracting PCs
# from the correlation matrix
#
# First, read in the data and check if OK
library(readxl)
Stock_data <- read_excel("<LOCATION OF SOURCE FOLDER>/Stock data.xlsx")
View(stock_data)
#
# Basic PCA using all standardized (TRUE) numeric vars (cols 2-4)
PCA.Stock <- prcomp(stock_data[2:4], scale=TRUE)
#
# Names of variables in the PCA object
names(PCA.Stock)
#
# Relative importance of the PCs
# Proportion of variance explaine
summary(PCA.Stock)
PCA.Stock$sdev^2
#
# Eigenvectors (PCs) of the correlation matrix of the data
PCA.Stock$rotation
#
# Scree plot
plot(PCA.Stock,type="lines")
#
# biplot of PC1 v PC2; scale=0 makes arrows scaled to rep loadings
biplot(PCA.Stock, scale = 0)
#
# PC scores for each observation (company)
PCA.Stock$x
#
# Correlation matrix of the data
m = cor(stock_data[2:4])
m # Print m
#
# Eigenvalues and eigenvectors of m
eigen(m)
