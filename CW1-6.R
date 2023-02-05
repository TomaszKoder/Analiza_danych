library('nlstools')
library('dplyr')

#data
v <- c(10, 16.3, 23, 27.5, 31, 35.6, 39, 41.5, 42.9, 45, 46, 45.5, 46, 49, 50)
t <- c(1:length(v))
data.set <- data.frame(t, v)
plot(v ~ t, data = data.set, pch = 20, xlim = c(0, 20), ylim = c(0, 60))

#non-linear model
skydive_nlmodel <- nls(v ~ SSmicmen(t, a, b), data = data.set)
summary(skydive_nlmodel)

#curve
curve(SSmicmen(x, coef(skydive_nlmodel)[1], coef(skydive_nlmodel)[2]), add = TRUE, col = 'blue', lwd = 2)

new.value <- predict(skydive_nlmodel, data.frame(t=17))
new.value
#51.85 m/s in 17th sec of movement 

#add new value to the plot
points(x = 17, y = new.value[1], pch = 10, col = 'red', cex = 2)
                                    
                                    #########
                                    #Summary#
                                    #########

#According to Michaelis-Menten model, velocity of sky-diver in 17s of movement should be 51.8m/s. 
#New value has been added to plot as a red pointer. 
