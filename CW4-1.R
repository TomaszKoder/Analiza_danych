library(dplyr)
library(vegan)
library(cluster)

require(ggplot2)

#załadowanie danych
data.set <- mtcars

#skalowanie
boxplot(mtcars)
data.set <- data.set %>% scale(center = TRUE, scale = TRUE) 

#hierarchiczna analiza skupień
distances <- dist(data.set, method = 'euclidean')
tree.average <- hclust(distances, method = 'average')
plot(tree.average, hang = -1)
lab.average <- cutree(tree.average, 5)
rect.hclust(tree.average, k = 5, border = 2, cluster = lab.average)

#ustalenie liczby clastrów                
cars.cascade <- cascadeKM(data.set, inf.gr = 2, sup.gr = 10, iter = 10, criterion = 'calinski')
cars.cascade$results
plot(cars.cascade)


#nie-hierarchiczna analiza skupień (k-means)
cars.kmeans <- kmeans(data.set, centers = 4, nstart = 100)
cars.kmeans$cluster
cars.kmeans$size

sil.index <- silhouette(cars.kmeans$cluster,
                        dist = dist(data.set, 
                                    method = 'euclidean'))
sil.index
summary(sil.index)
mean(sil.index[, 3])
plot(sil.index)
