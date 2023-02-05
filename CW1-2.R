library('dplyr')

apartments = data.frame('cena' = c(300, 250, 400, 550, 317, 389, 425, 289, 389, 559),
                        'liczba pokoi' =c(3, 3, 4, 5, 4, 3, 6, 3, 4, 5))

data.set <- apartments

plot(cena ~ liczba.pokoi, data = data.set, pch = 20)

#linear regression
apartments_model <- lm(cena ~ liczba.pokoi, data = data.set)
abline(apartments_model, lwd = 1.5, col = 'red')
summary(apartments_model)

#coefficiency tests w/o potential outliter
#Perason
cor.test(data.set$cena, data.set$liczba.pokoi, method = 'pearson')

#residuals
resid(apartments_model)
hist(resid(apartments_model), col = 'grey')
shapiro.test(resid(apartments_model))
plot(apartments_model, 5, pch = 20)

#observation no. 7 is a potential outliter
data.set[7,]

#Perason
cor.test(data.set$cena, data.set$liczba.pokoi, method = 'pearson', conf.level = 0.95)

# cor = 0.736 - quite strong positive correlation
# p-value = 0.01521 statistically significant

#Spearman
cor.test(data.set$cena, data.set$liczba.pokoi, method = 'spearman', conf.level = 0.95)

# rho = 0.8511649 
# p-value = 0.001787

new.data <- data.frame(liczba.pokoi = c(2))

#price projection for 2-rooms apartment                    
predict(apartments_model, new.data)                     
#240.6 kPLN

#removing observation no.7 as outliter
data.set2 <- data.set[-7,]
data.set2

#steps repeated

#linear regression
plot(cena ~ liczba.pokoi, data = data.set2, pch = 20)
apartments_model2 <- lm(cena ~ liczba.pokoi, data = data.set2)
abline(apartments_model2, lwd = 1.5, col = 'red')
summary(apartments_model2)
#R-squared and adjusted R-squared improved

#residuals
resid(apartments_model2)
hist(resid(apartments_model2), col = 'grey')
shapiro.test(resid(apartments_model2))
plot(apartments_model2, 5, pch = 20)

#coefficiency tests w/o potential outliter

#Perason
cor.test(data.set2$cena, data.set2$liczba.pokoi, method = 'pearson', conf.level = 0.95)

# cor = 0.8802 - quite strong positive correlation
# p-value = 0.001741 statistically significant 

#Spearman
cor.test(data.set2$cena, data.set2$liczba.pokoi, method = 'spearman', conf.level = 0.95)

# rho = 0.8454028
# p-value = 0.004098

#new price projection for 2-room apartment 
predict(apartments_model2, new.data)
#175.8 kPLN

                                #########
                                #Summary#
                                #########

#Both Pearson's product-moment correlation and Spearman's rank correlation 
#defines realtion between rooms and prices as significant and in expected direction
#at confidence level 95%. 
#However one potential outliter has been itentified. After removing it from data set
#model parameters have been improved.

#price projection for 2-room apartment is:
#240.6 kPLN before removing outliter 
#175.8 kPLN after removing outliter

#model assumptions are met. 