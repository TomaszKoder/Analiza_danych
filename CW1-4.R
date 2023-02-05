library('UsingR')
data.set <- homeprice

plot(sale ~ half, data = data.set, pch = 20, lwd = 0.75)
sale_model <- lm(sale ~ half, data = data.set)
abline(sale_model, lwd = 1, col = 'green')

cor.test(data.set$half, data.set$sale, method = 'pearson')

summary(sale_model)

                                  #########
                                  #Summary#
                                  #########

#According to Pearson's product-moment correlation, 
#p-value is equal to 0.03436 and correlation coefficient is equal to 0.3941621. 
#Correlation is in expected direction but very slight.
#With confidence level 95% null-hypotesis can be rejected. 
#Model parameters are very weak however. 
#Model assumptions have been not met. 
