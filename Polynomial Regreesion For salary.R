##############################################################################
##                        Polynomial Regression                             ##
##############################################################################

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')

# View the first 6 row of the data set 
head(dataset)

# structure of the dataset 
str(dataset)

# Data is two samll so no need to create data partition 
# well use whole data set for training

# Select only 2 variable i.e level and salary because position is nothing but the level 
dataset = dataset[2:3]
attach(dataset)

# Make a scatter plot of Level vs Salary 
plot(x = Level , y=Salary , las=1)
# The relationship between Level and Salary is no liner or curve 

# but first wll create liner model and  nonliner model then 
# well decide which one is best 

# Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)

# Add the liner model in scatter plot 
abline(lin_reg, lwd=2, col="red")


# check the summury
summary(lin_reg)

# Now create one polynomial regression model but before that
# we havet to add another variable which is nothing but the 
# our independent variable (Level) with power of 2,3.. and so on


dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4

# Increase the power of variable till you get better fit line 

head(dataset)


# Fitting Polynomial Regression to the dataset
poly_reg = lm(formula = Salary ~ .,
              data = dataset)
summary(poly_reg)

# In liner model the accuracy is 66%
# but in Ploynomial model the accuracy 99%
# so our second model is better than first model

# Cross check with partial F-test (anova)
# HO : There is no significant difference between two model 
# Ha : The full model (one with Level^2 ) is signinficantly better.

anova(lin_reg,poly_reg)

# P value < 0.05 
# Reject the Ho 
# Polynomial model is significantly better


# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle("  Linear Regression") +
  xlab('Level') +
  ylab('Salary')
# The residual is very large beacause the regression line dosent fit properly


# Visualising the Polynomial Regression results

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle("Polynomial Regression") +
  xlab('Level') +
  ylab('Salary')

# Residual is very low bcoz curve line fit properly


# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)

# Create sequence of level eg. Level = 0.1,0.2,0.3...,10
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)


ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle("Polynomial Regression") +
  xlab('Level') +
  ylab('Salary')



# Prediction on another test data
# If the Level is 6.5 then predict the expected salary for the employee


# Predicting a new result with Linear Regression
predict(lin_reg, data.frame(Level = 6.5))

# Salary = 330378 but this model is only 66% accurate 


# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))
# Salary = 158862 and this model is 99% accurate wo well accept this result 


################################## THE END #############################################
