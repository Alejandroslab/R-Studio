# Chapter 8 Lab: Decision Trees

# Fitting Classification Trees

library(tree)
library(ISLR)
#informazioni sulle variabili
?Carseats
plot(Carseats)
summary(Carseats)
#definizione variabile risposta (QLT) se n. vendite elevate se >8000 pezzi 
Carseats$High<-factor(ifelse(Carseats$Sales<=8,"No","Yes"))
#stima albero di classificazione
tree.carseats<-tree(High~.-Sales,data=Carseats)
summary(tree.carseats)
#attenzione devianza e' quella relativa a GLM (log verosimiglianza multinomiale)
#rappresentazione grafica
plot(tree.carseats)
text(tree.carseats,pretty=0)
#descrizione dell'albero: digitando solo il nome dell'oggetto corrispondente all'albero

#per ogni ramo si hanno informazioni su:

# criterio utilizzato per suddivisione (split) del nodo node)

# numero di osservazioni che figurano in quel ramo

# devianza

# valore previsto per la risposta in quel ramo (SI o NO) e percentuale di soggetti con valore (SI e NO)

tree.carseats

#stima del test error tramite validation set
set.seed(2)
train<-sample(1:nrow(Carseats), 200)
#test data set
Carseats.test<-Carseats[-train,]
High.test<-Carseats$High[-train]
#stima su training data set
tree.carseats<-tree(High~.-Sales,data=Carseats,subset=train)
#previsione su test data set
tree.pred<-predict(tree.carseats,Carseats.test,type="class")
#classification table
table(tree.pred,High.test)
#percentuale classificazioni corrette
(86+57)/200
#utilizzo cross validation per vedere se potatura (pruning) dell'albero migliora performance della classificazione
set.seed(3)
#la potatura viene effettuata considerando come criterio il classification error rate (di default FUN=prune.tree utilizza la devianza)
cv.carseats<-cv.tree(tree.carseats,FUN=prune.misclass)
#la funzione restituisce il numero di nodi di ciascun albero preso in considerazione, il tasso di errore (riportato come dev) e il valore del parametro costo/complessità utilizzato k che corrisponde ad alpha nella formula (8.4)
names(cv.carseats)
cv.carseats
#l'albero con 9 nodi terminali ha associato minimo tasso di errore di classificazione
layout(matrix(1:2,1,2))
plot(cv.carseats$dev~cv.carseats$size,type="b")
plot(cv.carseats$dev~cv.carseats$k,type="b")
#la funzione prune.misclass consente di potare l'albero tree.carseats considerato nel validation set approache, in modo da ottenere 9 nodi (foglie) terminali
prune.carseats<-prune.misclass(tree.carseats,best=9)
#rappresentazione grafica
plot(prune.carseats)
text(prune.carseats,pretty=0)
#performance sul test data set
tree.pred<-predict(prune.carseats,Carseats.test,type="class")
#classification table
table(tree.pred,High.test)
#percentuale classificazioni corrette
(94+60)/200
#utilizzando un albero di dimensione maggiore si ottiene una performance peggiore
prune.carseats<-prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred<-predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200








# Fitting Regression Trees

library(MASS)
library(tree)
?Boston
#stima albero di classificazione
tree.boston<-tree(medv~.,data=Boston)
summary(tree.boston)
#attenzione devianza e' quella relativa a GLM (log verosimiglianza multinomiale)
#rappresentazione grafica
plot(tree.boston)
text(tree.boston,pretty=0)
#descrizione dell'albero: digitando solo il nome dell'oggetto corrispondente all'albero

#per ogni ramo si hanno informazioni su:

# criterio utilizzato per suddivisione (split) del nodo node)

# numero di osservazioni che figurano in quel ramo

# devianza (MSE in corrispondenza dell'albero)

# valore previsto per la risposta in quel ramo (media di Y)
tree.boston


#stima di MSE tramite validation set
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
#stima su training data set e rappresentazione grafica
tree.boston<-tree(medv~.,Boston,subset=train)
summary(tree.boston)
#solo 3 variabili sono utilizzate nella costruzione dell'albero

# lstat    percent of the population with lower socioeconomic status

# rm    average number of rooms per dwelling

#dis    weighted mean of distances to five Boston employment centres
plot(tree.boston)
text(tree.boston,pretty=0)

#utilizzo cross validation per vedere se potatura (pruning) dell'albero migliora MSE
cv.boston<-cv.tree(tree.boston)
#la potatura viene effettuata considerando la devianza (criterio di default)
#la funzione restituisce il numero di nodi di ciascun albero preso in considerazione, la devianza e il valore del parametro costo/complessità utilizzato k che corrisponde ad alpha nella formula (8.4)
cv.boston
#l'albero con 8 nodi terminali (quello piu' complesso) ha associato minimo MSE
plot(cv.boston$size,cv.boston$dev,type='b')
#nel caso si desiderasse potare l'albero, questo e' possibile mediante la funzione prune.misclass
prune.boston<-prune.tree(tree.boston,best=5)
#rappresentazione grafica
plot(prune.boston)
text(prune.boston,pretty=0)
#performance sul test data set
yhat<-predict(tree.boston,newdata=Boston[-train,])
boston.test<-Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
#calcolo MSE test su test data set
mean((yhat-boston.test)^2)

# Bagging and Random Forests

library(randomForest)
set.seed(1)
#Bagging=Random Forest con m=p
#mtry=n. variabili considerate a ogni split dell'albero
#importance=TRUE calcola il valore di importanza per ciascun predittore
bag.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
#performance di questo modello bagged sul test data set
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#confrontare con mse ottenuto sull'albero potato inizialmente

#e' possibile stabilire il numero di alberi che vengono considerati dall'algoritmo di bagging (replicazioni bootstrap)
bag.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

#random forest: a ogni split dell'albero si considera un n. m<p di variabili esplicative
set.seed(1)
rf.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf <- predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
#leggero miglioramento

#qual e' l'importanza di ciascuna variabile?
importance(rf.boston)
#sono riportate 2 misure:

#la prima considera il decremento medio nell'accuratezza della previsione negli out of bag samples escludendo una variabile dal modello

#la seconda misura il decemento medio (rispetto a tutti gli alberi) totale nella impurezza del nodo che risulta splittando in base a tale variabile (per gli alberi di regressione training RSS, per alberi di classificazione devianza ml)

#rappresentazione grafica
varImpPlot(rf.boston)





# Boosting

library(gbm)
set.seed(1)
#opzione gaussian per distribuzione dal momneto che si considera problema riguardante alberi di regressione (per problemi di classificazione si utilizza bernoulli)
boost.boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
#summary produce influence plot e output con misure di influence
summary(boost.boston)
#le variabili piu' importanti sono lstat ed rm

#si possono costruire dei grafici di 'dipendenza parziale' che evidenziano l'effetto marginale di queste variabili al netto di tutte le altre
layout(matrix(1:2,1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

#utilizzo del modello boosted per previsione sul test data set
yhat.boost<-predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
#calcolo di MSE test (valore simile a random forest)
mean((yhat.boost-boston.test)^2)

#e' possibile modificare il valore del parametro shrinkage lambda nella (8.10)
boost.boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost<-predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)


