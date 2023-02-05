library('carData')
library('nlstools')

USPop

data.set <- USPop

plot(data.set, pch = 20)

#non-linear model
SSlogis_model <- nls(population ~ SSlogis(year, Asym, xmid, scal), data.set)

#model parameters
summary(SSlogis_model)

#add fitted polyline to the plot
lines(data.set$year, fitted(SSlogis_model), lwd = 2, col = 'blue')


#add curve to the plot
curve(SSlogis(x, coef(SSlogis_model)[1], coef(SSlogis_model)[2], coef(SSlogis_model)[3]), add = TRUE, col = 'red', lwd = 1.5)

AIC(SSlogis_model)
plot(nlsContourRSS(SSlogis_model), nlev = 10)
plot(nlsResiduals(SSlogis_model))
test.nlsResiduals(nlsResiduals(SSlogis_model))

                                    #########
                                    #Summary#
                                    #########

#Curve seems to fit well to data.
#Residuals SW normality test returns W = 0.88081 and p-value = 0.01252. For population 
#of 15 and confidence level 95% data is normally distributed. 
#Residual sum of squares surface countours shows no deviations. 
#Model assumptions are met.