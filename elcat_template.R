#Eclat - only uses support because it defines support as the set of products bought/watched together

#Data preprocessing
install.packages("arules")
library(arules)
dataset = read.csv("Market_Basket_Optimisation.csv", header = FALSE)
dataset = read.transactions(file = "Market_Basket_Optimisation.csv", 
                            sep = ",",
                            rm.duplicates = TRUE)
summary(dataset)

#Frequency Plot - topN us the number of categories in the plot
itemFrequencyPlot(dataset, topN = 50)

#Training Eclat on the dataset
rules = eclat(dataset, parameter = list(support = 0.03, minlen = 2))

#Visualising the data (need to sort by support)
inspect(sort(rules, by = 'support')[1:10])
