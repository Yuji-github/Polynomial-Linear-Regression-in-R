# import data 
data = read.csv('Position_Salaries.csv')

# Position and Level are the same 
data = data[2:3]

ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary), 
             color = 'red')+
  ggtitle('Salary vs Level (Simple Linear)') + 
  xlab('Level') + 
  ylab('Salary')

# the plot shows that linear can NOT follow the areas

# splitting data into x_train, x_test, y_train, y_test 
# this time we need to maximize the data 
# so, we do not need to have it
# set.seed(123)
# split = sample.split(data$Salary, SplitRatio = 2/3) #split(y, splitratio)
# train = subset(data, split == TRUE)
# test = subset(data, split == FALSE)

# training data (Simple Linear)
regressor = lm(formula = Salary ~ Level, data = data) 
summary(regressor)

# prediction 
y_pred = predict(regressor, newdata = data)
y_pred_six = predict(regressor, data.frame(Level = 6.5)) # showing 6.5 years value

# plotting the Linear Regression 
ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary), 
             color = 'red') +
  geom_line(aes(x = data$Level, y = predict(regressor, newdata = data)), 
            color = 'blue') +
  ggtitle('Salary vs Level (Simple Linear)') + 
  xlab('Level') + 
  ylab('Salary')

# training Polynomial Linear 
data$Level2 = data$Level^2 
data$Level3 = data$Level^3
data$Level4 = data$Level^4
poly_reg = lm(formula = Salary ~ ., data = data)
summary(poly_reg)

# predicting 6.5 years 
y_pred_poly = predict(poly_reg, newdata = data)
y_pred_poly_six = predict(poly_reg, data.frame(Level=6.5, Level2=6.5^2, Level3=6.5^3, Level4=6.5^4)) 

# plotting Polynomial Linear Regression 
ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary), 
             color = 'red') +
  geom_line(aes(x = data$Level, y = predict(poly_reg, newdata = data)), 
            color = 'blue') +
  ggtitle('Salary vs Level (Polynomial Linear)') + 
  xlab('Level') + 
  ylab('Salary')