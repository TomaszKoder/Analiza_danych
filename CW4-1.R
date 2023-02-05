library(dplyr)

#załadowanie danych
data.set <- mtcars

#skalowanie
boxplot(mtcars)
data.set <- data.set %>% scale(center = TRUE, scale = TRUE) 

distances <- dist(data.set, method = 'euclidean')

tree.average <- hclust(distances, method = 'average')

plot(tree.average, hang = -1)
lab.average <- cutree(tree.average, 5)
rect.hclust(tree.average, k = 5, border = 2, cluster = lab.average)
