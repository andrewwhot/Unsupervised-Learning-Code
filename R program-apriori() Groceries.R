#############################################
## Association Rules with apriori()        ##
##   "Groceries" dataset                   ## 
#############################################
# Load necessary libraries
# install.packages("arules")  # if not already installed
library(arules)
# install.packages(arulesViz) # if not already installed
library(arulesViz)


# Data must be processed into special format before using with aprior()
# The data can be thought of as transactions data, with a 
# transaction ID and list of items in the transaction.
# The data do not need to be purchase data, but it is simplest
# to learn about apriori() if they are.

# Examples of processing data from Excel into required format for apriori()
# Single column format and multiple column format for transactions
#  see example file "test_apriori_data.xlsx"

# SINGLE COLUMN FORMAT
# Read in test_single from first sheet in "test_apriori_data.xlsx"
# NOTE: data file can also be CSV with row format "1,milk"
# Convert the data to a transactions object
test_single_trans <- as(split(apriori$Item, apriori$TransactionID), "transactions")
# Inspect the transactions object
inspect(test_single_trans)
# Apply apriori()
test_rules_single <- apriori(test_single_trans, parameter = list(supp = 0.01, conf = 0.5))

# MULTIPLE COLUMN FORMAT
# Read in test_mult from second sheet in "test_apriori_data.xlsx"
# Convert the data to a transactions object
# Remove the TransactionID column
test_mult.m <- test_mult[,-1]
# Convert data to a list of transactions
test_mult.m_list <- apply(test_mult.m, 1, function(x) x[!is.na(x)])
# Convert the list to a transactions object
test_mult.m_trans <- as(test_mult.m_list, "transactions")
# Inspect the transactions
inspect(test_mult.m_trans)
# Apply apriori()
test_rules_mult <- apriori(test_mult.m_trans, parameter = list(supp = 0.01, conf = 0.5))

############################################################
# The Groceries dataset comes built into "arules". 
# It has real-world point-of-sale data from a grocery store
# collected over 30 days. There are 9,835 transactions (check-outs)
# involving 43,366 purchased items, of which 169 are unique.
# Interest focuses on finding relationships among the 169 unique items.
# Typically, unique occurrences of items are examined.
# But you may want to consider quantity as well ...
# E.g., a consumer can buy 2 six-packs of the same beer.
# Activate the Groceries dataset by ...
data("Groceries")
# Groceries has already been processed into the proper format for apriori()

# Generate association rules using the Apriori algorithm
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5))

# Inspect the top 5 rules
inspect(head(rules, 5))

# Typically, rules are sorted in order of confidence.
# But this can be changed. To sort explicitly:

# Inspect the top 5 rules sorted by confidence
inspect(sort(rules, by = "confidence")[1:5])
# Inspect the top 5 rules sorted by lift
inspect(sort(rules, by = "lift")[1:5])
# Inspect the top 5 rules sorted by support
inspect(sort(rules, by = "support")[1:5])

# Visualization of rules
# Scatter plot of rules
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

# Matrix plot of rules
plot(rules, method = "matrix", measure = "lift")

# Graph-based plot of rules
plot(rules, method = "graph", control = list(type = "items"))

# Grouped matrix plot of rules
plot(rules, method = "grouped")

# Parallel coordinates plot of rules
plot(rules, method = "paracoord", control = list(reorder = TRUE))

