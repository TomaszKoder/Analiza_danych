library(dplyr)
library(vegan)
library(cluster)
library(skimr)

require(ggplot2)
require(dendextend)

#załadowanie danych
data.set <- mtcars
skimr::skim(mtcars)

#skalowanie
boxplot(mtcars)
data.set <- data.set %>% scale(center = TRUE, scale = TRUE) 

#hierarchiczna analiza skupień
distances <- dist(data.set, method = 'euclidean')
tree.average <- hclust(distances, method = 'average')
plot(tree.average, hang = -1)
lab.average <- cutree(tree.average, 5)
rect.hclust(tree.average, k = 5, border = 2, cluster = lab.average) 


#dendrogram z kolorowymi gałęziami
col.dendrogram <- as.dendrogram(tree.average)
col.dendrogram %>% set("branches_k_color", value = 1:5, k = 5) %>% set("branches_lwd", 2) %>% plot()

sil.index.dendr <- silhouette(lab.average, 
                              dist = dist(data.set, 
                              method = 'euclidean'))

lab.average

sil.index.dendr
mean(sil.index.dendr[, 3])
plot(sil.index.dendr)

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

#to samo dla 5 skupień
cars.kmeans5 <- kmeans(data.set, centers = 5, nstart = 100)
cars.kmeans5$cluster


sil.index
summary(sil.index)
mean(sil.index[, 3])
plot(sil.index)

#Patrząc na dendrogram będący wynikiem hierarhicznej analizy skupień, podzieliłbym 
#zbiór danych na 5 grup. 
#Wartość średniego zarysu wyniosła 0.43.
#Natomiast indeks CH wskazuje, że optymalnym podziałem zbioru mtcars jest podział na 
#4 grupy. 
#Po przeprowadzeniu nie-hierarchicznej analizy skupień otrzymano wykres zarysu,
#ze średnią wartością 0.43.
#W obu przypadkach wartości średniego zarysu wyniosły 0.43, co jest dalekie od idealnej
#wartości 1.
#Po porównaniu etykiet uzyskanych dla obu metod klasyfikacji, okazuje się, że większość
#samochodów została pogrupowana tak samo w obu przypadkach. 
#Powodem małej rozbieżności wyników może być niska ilość danych w data secie
#czym rówież można tłumaczyć niską wartość średniego zarysu

