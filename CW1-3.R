library('UsingR')
library('dplyr')


#data
data.set <- emissions %>% as.data.frame()
data.set
plot(CO2 ~ GDP, data = data.set, pch = 20, cex = 0.75)


#linear model
CO2_model1 <- lm(CO2 ~ GDP, data = data.set)
abline(CO2_model1, lwd = 1.5, col = 'red')

#data.set lm evaluation
summary(CO2_model1)
#Multiple R-squared:  0.9028,	Adjusted R-squared:  0.8988
(MSE.CO2_model1 <- mean(CO2_model1$residuals^2))
cor.test(data.set$GDP, data.set$CO2, method = 'pearson')

#finding outliters
data1.resid <- resid(CO2_model1)
data1.resid
plot(CO2_model1, c(1:5), pch = 20)

#Despite it is observation for US that lies outise od Cooks's distances
#the biggest errors are for Japan and Russia. Also both these countries are 
#indicated as potential outliters on Q-Q plot.
#Let's remove them:

#removing potential outliter(s)
countries.to.remove <- c('Japan', 'Russia')
data.set2 <- data.set[!(row.names(data.set) %in% countries.to.remove),]

#data.set2 linear model
CO2_model2 <- lm(CO2 ~ GDP, data = data.set2)
abline(CO2_model2, lwd = 1.5, col = 'green')
legend('topleft', legend = c('CO2_model1', 'CO2_model2'), lwd = 2, cex = 0.75, col = c('red', 'green'))

#data.set2 lm evaluation
summary(CO2_model2)
#Multiple R-squared:  0.9774,	Adjusted R-squared:  0.9763
(MSE.CO2_model2 <- mean(CO2_model2$residuals^2))
cor.test(data.set2$GDP, data.set2$CO2, method = 'pearson')

plot(CO2_model2, c(1:5), pch = 20)

BIC(CO2_model1, CO2_model2)

                                  #########
                                  #Summary#
                                  #########

#After analyse of data distribution and linear regressions parameter, it has been
#decided to remove Japan and Russia from observations as a potential outliters.
#After that model parameters significantly improved.
#Also BIC picks model2 (w/o woutliter) as a better one.
#Next potential outliter to be removed is US.
