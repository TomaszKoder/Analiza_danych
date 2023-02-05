library(MASS)
library(tidyverse)

require(factoextra)
require(ggplot2)
require(ggfortify)

?Cars93

data.set <- Cars93 %>% as.tibble()

#Usunięcie wierszy z wartością n/a z data setu
data.set <- na.omit(data.set)

#Usunięcie Mazdy RX-7  data set, jako że ma silnik wankla, a nie tłokowy
data.set <- data.set[!(row.names(data.set) %in% c('Mazda RX-7')),]

#Usunięcie z data setu wierszy Model i Manufacturer, jako że te informacje są 
#zawarte również w kolumnie Make która została przeniesiona do nazw wierszy
data.set <- subset(data.set, select = -c(Manufacturer, Model)) %>% 
  column_to_rownames(var = 'Make')

#Usunięcie etykiet Origin i Type jako że będą one pzedmiotem analizy 
data.set2 <- subset(data.set, select = -c(Origin, Type))

#Zmiana typu danych w kolumnie Cylinders, jako że po usunięciu Mazdy RX-7
#z data setu zostały w tej kolumnie wyłącznie wartości liczbowe 
data.set2$Cylinders <- as.numeric(data.set2$Cylinders)

#transformacja wartości porządkowych na ciągłe 
data.set2 <- model.matrix(~-1 + ., data = data.set2)

skimr::skim(data.set2)

boxplot(data.set2)
#dane muszą być przeskalowane

#PCA:
data.set2.PCA <- prcomp(data.set2, scale. = TRUE)
summary(data.set2.PCA)
screeplot(data.set2.PCA, type = 'lines', pch = 20)
data.set2.PCA$rotation
#blisko 80% wariancji jest objaśnione przez 4 pierwsze składowe głowne 

#biplot bez grup wyszczególnionych kolorami
biplot(data.set2.PCA, cex = 0.4, expand = 1, choices = c(3,4))

#z pakietem factoextra
fviz_pca_biplot(data.set2.PCA, col.ind = data.set$Origin, labelsize = 2, addEllipses = TRUE)

fviz_pca_biplot(data.set2.PCA, col.ind = data.set$Type, labelsize = 2, addEllipses = TRUE)

#z pakietem ggplot2
#dla dwóch pierwszych składowych PC1 i PC2
autoplot(data.set2.PCA, data = data.set, colour = "Origin", 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 2,
         frame = TRUE, frame.type = 'norm')

autoplot(data.set2.PCA, data = data.set, colour = "Type", 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 2,
         frame = TRUE, frame.type = 'norm')

#dla PC3 i PC4
autoplot(data.set2.PCA, data = data.set, colour = "Origin", 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 2,
         frame = TRUE, frame.type = 'norm', x = 3, y = 4)

autoplot(data.set2.PCA, data = data.set, colour = "Type", 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 2,
         frame = TRUE, frame.type = 'norm', x = 3, y = 4)

#Wnisoki:
#Analizując dwie pierwsze składowe PC1 i PC2 można dojść do następujących wniosków: 
#Amerykańskie samochody cechują się większymi gabarytami. Dominują cechy takie jak 
#masa, długośc, ilość pasażerów czy miejsce na bagaż. Mają również większe sliniki.
#Samochody poza Stanami Zjednoczonymi są natomiast droższe.
#Zaskakującym może być fakt, że nie dostrzeżono większych różnic w spalaniu
#Analiza dwóch pierwszych składowych pod względem typu samochodu prowadzi do 
#następujących wniosków: auta z segmentu Large mają większe gabaryty oraz silniki.
#Auta kompaktowe stanowią pewien balans, pomiędzy wszystkimi chechami z tendencją
#do posiadania napędu na przednią oś, oraz manualnej skrzyni biegów. 
#Samochody segmentu Midsize cechuje zazwyczaj wysoka cena,oraz moc. Posiadają także
#poduszki powietrzne dla kierowcy i pasażera. 
#Auta Sporty to przede wszystkim wysoka cena, napęd na tylną oś, wysokie obroty,
#spalanie oraz moc. 

