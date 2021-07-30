
# Installation of the Packages

install.packages("dplyr")
install.packages("arules")
install.packages("plotly")
install.packages("arulesViz")
install.packages("visNetwork")
install.packages("igraph")
install.packages("reshape2")
install.packages("readxl")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")  
# Importing Libraries

library(readxl)
library(dplyr)
library(arules)
library(plotly)
library(arulesViz)
library(visNetwork)
library(igraph)
library(reshape2)
# alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(arules)


df <- read_excel("input.xls", sheet = "Data")

# Change the description to lowercase
df$Insurance_Description <- tolower(df$Insurance_Description)

# Specify columns to work with
required_cols <- c("Mosaic_Class", "Insurance_Description")

# Filter data and select only required columns. Filtering on PPI=1 as only
# they have product info.

required_df <- df %>% filter(PPI == 1) %>% select(required_cols)

# Wrange data to be usable as transaction data
products <- aggregate(Insurance_Description ~ Mosaic_Class, required_df, c)

transactions <- as(products$Insurance_Description, "transactions")

# Run apriori algorithm to get rules
rules <- apriori(transactions, parameter = list(supp = 0.006, conf = 0.25, minlen = 2))

# Sort the rules according to decreasing confidence
rules <- sort(rules, by = "confidence", decreasing = TRUE)

# Remove the redundant rules
rules <- rules[!is.redundant(rules)]
rules_df <- as(rules, "data.frame")

# Output the rules into a csv file
write.csv(rules_df, "output.csv")

summary(rules)
inspect(rules[1:10])



# Filter rules with confidence greater than 0.6 or 60%
subRules<-rules[quality(rules)$confidence>0.6]
#Plot SubRules
plot(subRules)

plot(subRules,method="two-key plot")

# Graph

top10subRules <- head(subRules, n = 10, by = "confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")

saveAsGraph(head(subRules, n = 1000, by = "confidence"), file = "rules.graphml")
