cars
data.set <- cars
plot(dist ~ speed, data = data.set, pch = 20)

#Linear model
car_model <- lm(dist ~ speed, data = data.set)
summary(car_model)
abline(car_model, lwd = 1.5, col = 'blue')

hist(resid(car_model), col = 'orange')
shapiro.test(resid(car_model))
summary(car_model)

#Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438

#MSE for linear model:

(MSE.car_model <- mean(car_model$residuals^2))

#Quadratic model
car_model_sq <- lm(dist ~ speed + I(speed^2), data = data.set)
summary(car_model_sq)
lines(data.set$speed, predict(car_model_sq), lwd = 1.5, col = 'green')

summary(car_model_sq)

#Multiple R-squared:  0.6673,	Adjusted R-squared:  0.6532 

#MSE for quatdratic model

(MSE.car_model_sq <- mean(car_model_sq$residuals^2))

#Cubic model
car_model_cubic <- lm(dist ~ speed + I(speed^2) + I(speed^3), data=data.set)
lines(data.set$speed, predict(car_model_cubic), lwd = 1.5, col = 'red')

summary(car_model_cubic)

#Multiple R-squared:  0.6732,	Adjusted R-squared:  0.6519

#MSE for cubic model

(MSE.car_model_sq <- mean(car_model_cubic$residuals^2))

legend('topleft', legend = c('Linear model', 'Quadratic model', 'Cubic model'),
       lwd = 2,
       cex = 0.75,
       col = c('blue', 'green', 'red'))

AIC(car_model, car_model_sq, car_model_cubic)
#Akaike criterion picks model3

BIC(car_model, car_model_sq, car_model_cubic)
#Bayesian criterion picks model1
                
                                    #########
                                    #Summary#
                                    #########

#Akaike information criterion picks model3, however differences between values 
#for all 3 models are not significant.

#Bayesian information criterion picks model1.

#Adjusted R-squared slightly improves for quadratic model and slightly 
#decreasing for cubic model

#Due to slight/not significant differences between all 3 model, it has been 
#decided to pick the simplest one - linear model.
