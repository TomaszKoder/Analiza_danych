library('MASS')
library('tidyverse')

?painters

#Załadowanie danych
data.set <- painters %>% as.data.frame()
data.set
plot(data.set, pch = 20)


#Usunięcie etykiety z danych (School)
data.set2 <- data.set[,!colnames(data.set) %in% 'School']

boxplot(data.set2)
diag(var(data.set2))
#Wszystkie punkty widocznie były przyznawane w tej samej skali

painters.pca <- prcomp(data.set2)
painters.pca
biplot(painters.pca)
summary(painters.pca)

#Dwie pierwsze składowe głowne wyjaśniają 0.84% wariancji
#Pierwsze trzy sładowe głowne wyjaśniają 0.94% wariancji

painters.pca$rotation

school.colors = c(1:8)
plot(painters.pca$x, pch = 20, col = school.colors[data.set$School])
legend('bottomleft', 
       legend = c('A: Renaissance', 'B: Mannerist', 'C: Seicento', 'D: Venetian', 'E: Lombard', 'F: Sixteenth Century', 'G: Seventeenth Century', 'H: French'),
       pch = 20,
       cex = 0.4,
       col = school.colors,
       bty = 'n'
)
