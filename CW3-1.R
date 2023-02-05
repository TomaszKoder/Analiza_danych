library(mlbench)
library(caret)
library(dplyr)
library(MASS)

require(microbenchmark)
require(ggplot2)
require(ggfortify)


#Ładowanie danych
?Vehicle
data(Vehicle)
summary(Vehicle)
data.set <- Vehicle

set.seed(1000)

fit.control <- trainControl(method = 'cv',
                            number = 10
                            )

#KNN
knn.fit <- train(Class ~ .,
                 data = data.set,
                 method = 'knn',
                 trControl = fit.control,
                 preProc = c('scale', 'center'),
                 tuneGrid = data.frame(k=1)
                )

knn.fit$results$Accuracy
knn.fit$results$Kappa

knn.imp <- varImp(knn.fit, scale = FALSE)
plot(knn.imp)

#LDA
lda.fit <- train(Class ~ .,
                 data = data.set,
                 method = 'lda',
                 trControl = fit.control, 
                 preProc = c('scale', 'center')
                 )

lda.fit$results$Accuracy
lda.fit$results$Kappa


#QDA
qda.fit <- train(Class ~ .,
                 data = data.set,
                 method = 'qda',
                 trControl = fit.control, 
                 preProc = c('scale', 'center')
                 )

qda.fit$results$Accuracy
qda.fit$results$Kappa

#NB
nb.fit <- train(Class ~ .,
                data = data.set,
                method = 'nb',
                trControl = fit.control, 
                preProc = c('scale', 'center'),
                tuneGrid = data.frame(fL = 0, usekernel = TRUE, adjust = 1)
)

nb.fit$results$Accuracy
nb.fit$results$Kappa

Results <- data.frame(Method = c('KNN', 'LDA', 'QDA', "NB"),
                      Accuracy = c(knn.fit$results$Accuracy, lda.fit$results$Accuracy, qda.fit$results$Accuracy, nb.fit$results$Accuracy),
                      Kappa = c(knn.fit$results$Kappa, lda.fit$results$Kappa, qda.fit$results$Kappa, nb.fit$results$Kappa))

#Zestawienie wyników Accuracy i Kappa dla wszystkich klasyfikatorów
Results

#Próba sprawdzenia czasu wykonania kodu dla każdego klasyfikatora
microbenchmark(knn.fit, lda.fit, qda.fit, nb.fit, times = 1000)

#Klasyfikator QDA wykazał najwięszką dokładność spośród wszystkich badanych metod klasyfikacji. 
#Czas wykonania kodu sprawdzony za pomocą biblioteki microbenchmark, nie wykazuje istotnych różnic, pomiędzy
#poszczególnymi klasyfikatorami. Być może ze względu na ilość danych z zbiorze. 
#W związku z powyższym jako klasyfiator godny polecenia dla tego zbioru danych wybieram QDA. 

#Wizualizacja 
data.pca <- dplyr::select(data.set, -c('Class'))
vehicle.pca <- prcomp(data.pca, scale = TRUE)

qda.predict <- predict(qda.fit, newdata = data.pca)
qda.predict

confusionMatrix(qda.predict, data.set$Class)
#Najczęsciej mylone klasy to 'Saab' i 'Opel'

data.set <- data.set %>% mutate('predictions' = qda.predict)

autoplot(vehicle.pca, data = data.set, colour = 'Class', shape = 'predictions') + labs(colour = 'Pierwotne dane', shape = 'Klasyfikacja QDA')
