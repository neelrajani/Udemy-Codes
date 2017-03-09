#Apriori

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

#Training Apriori on the dataset (0.003 is buying stuff 3 times a day) - https://www.udemy.com/machinelearning/learn/v4/t/lecture/6455322
rules = apriori(dataset, parameter = list(support = 0.003, confidence = 0.4))

#Visualising the data
inspect(sort(rules, by = 'lift')[1:10])
