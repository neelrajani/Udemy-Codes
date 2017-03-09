#SVR

#Importing Data
dataset = read.csv('Position_Salaries.csv')

#Don't need first column as second column factorises first column in this example
dataset = dataset[,2:3]

#Missing Data (None here)
#dataset$Salary = ifelse(is.na(dataset$Salary),
#                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
#                        dataset$Salary)

#Encoding Categorical Data (Already done here so not needed)
#dataset$Purchased = factor(dataset$Purchased,
#                        levels = c("Yes","No"),
#                        labels = c("1","0"))

#Splitting Data into Training and Test (Only got ten samples so not here)
#set.seed(123)
#split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

# Feature Scaling - Must always do this in SVR
# training_set = scale(training_set)
# test_set = scale(test_set)

#Fitting Regression Model to Dataset
install.packages("e1071")
library(e1071)
regressor = svm(Salary ~ ., 
                data = dataset,
                type = 'eps-regression')

# Visualising the SVR Regression Model results
# install.packages('ggplot2')
#library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR Regression Model)') +
  xlab('Level') +
  ylab('Salary')