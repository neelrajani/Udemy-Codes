#Polynomial Regression Model

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

#Fitting Polynomial Model to Dataset (can add as many as you want)
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
poly_reg = lm(Salary ~ ., 
           data = dataset)
summary(poly_reg)

# Visualising the Polynomial Regression results
# install.packages('ggplot2')
#library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

#Predicting a new result with Polynomial Regression (Here we are predicting 6.5 level)
y_pred = predict(poly_reg,data.frame(Level = 6.5,
                                     Level2 = 6.5^2,
                                     Level3 = 6.5^3))