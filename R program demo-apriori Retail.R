###########################################
# apriori() Association Rules example     #
#  "Online Retail.xlsx" store data        #
###########################################
# Read "Online Retail.xlsx" data into retail
# Explore the data

length(unique(retail$InvoiceNo))           
length(unique(retail$StockCode))           
length(unique(retail$Description))         
length(unique(retail$CustomerID))          
length(unique(retail$Country))              

# find top 10 items
head(sort(table(retail$StockCode),decreasing=TRUE),10)
c1 

# get matching descriptions
# Keep only the first match in column2 for each unique value in column1
# Subset retail to include only the top 10 StockCodes
subset_retail <- retail[retail$StockCode %in% c1, ]
# Keep only the first match in Description for each of top 10 StockCodes
desc_match <- subset_retail[!duplicated(subset_retail$Description), ]

# Convert the data to a transactions object
retail_trans <- as(split(retail$StockCode, retail$InvoiceNo), "transactions")
# Inspect the transactions object
# inspect(retail_trans) # takes too long
# Apply apriori()
retail_rules <- apriori(retail_trans, parameter = list(supp = 0.01, conf = 0.5))

# Inspect the top 5 rules
inspect(head(retail_rules, 5))

# Inspect the top 5 rules sorted by confidence
inspect(sort(retail_rules, by = "confidence")[1:5])
# Inspect the top 5 rules sorted by lift
inspect(sort(retail_rules, by = "lift")[1:5])
# Inspect the top 5 rules sorted by support
inspect(sort(retail_rules, by = "support")[1:5])
# Inspect the top 5 rules sorted by coverage
inspect(sort(retail_rules, by = "coverage")[1:5])
