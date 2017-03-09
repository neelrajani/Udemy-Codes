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

#Splitting Data into Training and Test (The Decision Tree algorithm does not need splitting as it doesn't depend on Euclidian distance)
#set.seed(123)
#split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

#Fitting Random Forest Regression Model to Dataset
#the 'x' argument needs to be a dataframe and 'y' needs to be a vector
install.packages("randomForest")
library(randomForest)
set.seed(1234)
regressor = randomForest(data = dataset,
                         x = dataset[1],
                         y = dataset$Salary,
                         ntree = 500)

# Visualising the Random Forest Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff Random Forest Regression Model)') +
  xlab('Level') +
  ylab('Salary')