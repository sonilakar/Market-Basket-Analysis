library(dplyr)
library(tidyr)
library(reshape2)
library(arules)
  
retail_data <- read.csv("Online Retail.csv")

retail_data_subset <- retail_data[,c(1,3)]

retail_data_subset <- filter(retail_data_subset, Description != "")

#retail_data_subset <- sample_n(retail_data_subset, 10000)

retail_data_subset$prod_count <- 1

retail_data_subset2 <- retail_data_subset %>%
  group_by(InvoiceNo, Description) %>%
  summarise(prod_count = sum(prod_count))

#retail_data_subset3 <- retail_data_subset %>%
#  group_by(Description) %>%
#  summarise(n())

data_wide <- spread(retail_data_subset2, Description, prod_count)

data_wide <- data_wide[-1]

data_wide[is.na(data_wide) == FALSE] <- 1
data_wide[is.na(data_wide) == TRUE] <- 0

col_names <- names(data_wide)
data_wide[,col_names] <- lapply(data_wide[,col_names] , factor)

rules <- apriori(data_wide, 
                 #parameter = list(minlen = 2,
                 #maxlen = 3,
                 #supp = 0.5),
                 #conf=0.9))
                 appearance = list(rhs=c("REGENCY CAKESTAND 3 TIER=1"),
                                   default="lhs"))

inspect(rules)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

retail_data <- read.csv("small retail data.csv")
col_names <- names(retail_data)
retail_data[,col_names] <- lapply(retail_data[,col_names] , factor)

rules <- apriori(retail_data, 
                 parameter = list(maxlen = 3, conf = 0.8),
                 appearance = list(rhs=c("Bread=1"), default="lhs"))

inspect(rules)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#with Groceries data

data(Groceries)

grocery_rules <- apriori(Groceries)
grocery_rules <- apriori(Groceries, 
                         parameter = list(supp = 0.01, conf=0.5))

inspect(grocery_rules)

grocery_rules_sorted <- sort(grocery_rules, by = "support", decreasing = T)

inspect(grocery_rules_sorted)

is.redundant(grocery_rules_sorted)


















