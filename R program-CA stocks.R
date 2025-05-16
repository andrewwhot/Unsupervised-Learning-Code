# Cluster analysis of stocks data
library(readxl)
stocks <- read_excel("<YOUR FOLDER LOCATION>/Stock data.xlsx")
View(stocks)
library(mosaic) # needed later

# First compute Euclidean distances between firms
dist_stocks <- dist(stocks[2:4], method="euclidean")
# The 3 clustering vars have similar scales,
# so it does not matter much if they are standardized
# before clustering. Ordinarily, I would standardize anyway.
# But certain pedagogical points are better made with
# unstandardized data, so I will not standardize here.
# If you want to standardize, the following is an easy way:
stocksx <- as.data.frame(scale(stocks[2:4]))
# Then add the categorical vars (FIRM, Industry) back in:
stocksx <- cbind(stocksx,stocks[c(1,5)])

# Hierarchical agglomerative clustering (AGNES)
# Use Ward's method of clustering
# ward.D2 is Ward's original method; ward.D is a modern variant.
# ward.D2 squares dissimilarities before cluster updating.
CA.stocks <- hclust(dist_stocks,method="ward.D2")
 
# Display dendrogram
plot(CA.stocks)
# The height var shows the value of the clustering criterion 
# after each join in the agglomeration.
CA.stocks$height

# Select 3-cluster solution
cl_stocks3 = cutree(CA.stocks, k=3)
cl_stocks3
# Add cluster number to data
stocks <- cbind(stocks, cl_stocks3)

# Display how the clusters differ from each other - need mosaic for this
mean(P_E    ~ cl_stocks3, data=stocks)
mean(PROFIT ~ cl_stocks3, data=stocks)
mean(GROWTH ~ cl_stocks3, data=stocks)

# Test explanatory power of cluster memberships
# Create dummy variables for cluster memberships
cl1 <- ifelse(stocks$cl_stocks3 == 1, 1, 0)
cl2 <- ifelse(stocks$cl_stocks3 == 2, 1, 0)
cl3 <- ifelse(stocks$cl_stocks3 == 3, 1, 0)
# Add dummies to stocks
stocks <- data.frame(stocks, cl1=cl1, cl2=cl2, cl3=cl3)

# Explanatory power of cl1 for each clustering variable
reg1 <- lm(P_E    ~ cl1, data=stocks)
summary(reg1)
reg2 <- lm(PROFIT ~ cl1, data=stocks)
summary(reg2)
reg3 <- lm(GROWTH ~ cl1, data=stocks)
summary(reg3)

# Explanatory power of the entire clustering for each clustering variable
# Cannot use all CL1, CL2, CL3 b/c multicollinear
# Omit cl3, which becomes the baseline
reg11 <- lm(P_E    ~ cl1 + cl2, data=stocks)
summary(reg11)
reg21 <- lm(PROFIT ~ cl1 + cl2, data=stocks)
summary(reg21)
reg31 <- lm(GROWTH ~ cl1 + cl2, data=stocks)
summary(reg31)
# This is the same as ANOVA (analysis of variance)
