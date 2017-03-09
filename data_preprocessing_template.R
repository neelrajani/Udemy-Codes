#PreProcessing Data

#Importing Data
dataset = read.csv('Data.csv')

#Missing Data
dataset$Salary = ifelse(is.na(dataset$Salary),
                   ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$Salary)

#Encoding Categorical Data
dataset$Purchased = factor(dataset$Purchased,
                         levels = c("Yes","No"),
                         labels = c("1","0"))

#Splitting Data into Training and Test
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
training_set[,2:3] = scale(training_set[,2:3])
test_set[,2:3] = scale(test_set[,2:3])