# 8 Linear Models
library(tidyverse)
#Import the mpg.csv data.
mpg <- read_csv('mpg.csv')
#Fit a linear model to predict city fuel efficiency (city.mpg) on the basis of fuel type (fuel; gas = petrol), horse power (hp) and drive-chain (drive)
mdl2 <- lm(formula = city_mpg ~ fuel + hp + drive, data = mpg)
#Display the key results of the fitted model.
summary(mdl2)
#Check the assumptions for this model.

#Extract the parameter estimates and their 95% confidence intervals and interpret them.
print('coefficients of linear model:')
coef(mdl2)
print('residuals of linear model:')
head(resid(mdl2))
print('95% confidence intervals')
confint(mdl2, level = 0.95)

#Predict average city fuel efficiency for a 4wd petrol car with 300 horse-power.
dfPred <- data.frame(fuel = 'gas', 
                     hp = 300, 
                     drive = "4wd")
predict(mdl2, newdata = dfPred)


