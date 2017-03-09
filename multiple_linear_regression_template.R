#Multiple Linear Regression (Backward Selection)
# Need to make sure that the data is checked for following assumptions:
# Linearity, Homoscedasticity, Multivariate Normality, Independence of Errors, Lack of multicollinearity

#Importing Data
dataset = read.csv('50_Startups.csv')

#Missing Data - don't need to because there is no missing data in this example
#dataset$Salary = ifelse(is.na(dataset$Salary),
#                       ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
#                       dataset$Salary)

#Encoding Categorical Data
dataset$State = factor(dataset$State,
                           levels = c("New York","California","Florida"),
                           labels = c("1","2","3"))

#Splitting Data into Training and Test
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling - don't need to because the multiple linear regression function covers that for us
#training_set[,2:3] = scale(training_set[,2:3])
#test_set[,2:3] = scale(test_set[,2:3])

#Fitting Multiple Linear Regression Model to Training Data
regressor = lm(Profit ~ ., 
            data = training_set)

#Predicting the Test Results using Test Data
y_pred = predict(regressor, newdata = test_set)

#Optimising the model using Backward Elimination (SL theshold of 0.05)
regressor = lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
               data = dataset)
#Use entire dataset here
summary(regressor)

#Getting rid of independent variable: State
regressor = lm(Profit ~ R.D.Spend + Administration + Marketing.Spend, 
               data = dataset)
summary(regressor)

#Getting rod of independent variable: Administration
regressor = lm(Profit ~ R.D.Spend + Marketing.Spend, 
               data = dataset)
summary(regressor)

#Potentially getting rod of independent variable: Administration
regressor1 = lm(Profit ~ R.D.Spend, 
               data = dataset)
summary(regressor1)
