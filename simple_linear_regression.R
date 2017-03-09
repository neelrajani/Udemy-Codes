#Simple Linear Regression

#Importing Data
dataset = read.csv('Salary_Data.csv')

#Missing Data
#dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)

#Encoding Categorical Data
#dataset$Purchased = factor(dataset$Purchased,
                           levels = c("Yes","No"),
                           labels = c("1","0"))

#Splitting Data into Training and Test
set.seed(123)
split <- sample.split(dataset$YearsExperience, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling - don't need it for Simple Linear Regression model we are using
#training_set[,2:3] = scale(training_set[,2:3])
#test_set[,2:3] = scale(test_set[,2:3])

#Fitting Simple Linear Regression Model to Training Set
regressor = lm(formula = Salary ~ YearsExperience,data = training_set)
summary(regressor)

#Predicitng Test Set Results
y_pred = predict(regressor, newdata = test_set)

#Install plotting graph libraries
#install.packages("ggplot2")
#library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')