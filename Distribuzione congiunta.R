#Distribuzioni multidimensionali

rm(list=ls()) #per cancellare tutti gli elementi, tutte le variabili dal environment

#oggetto matrice con distribuzione congiunta
barattoli<-matrix(c(1,3,7,5,5,10,15,30,2,2,6,10,1,1,2),nrow=3, ncol=5, byrow=T,
dimnames=list(c("18","19","20"),c("0","1","2","3","4")))

#per visualizzare datasets
data()

#per visualizzare packages 
library()

#visualizzare dati relativi al dataset 'trees'
trees

#visualizzare dati in forma abbreviata in righe
str(trees)

#disegnare grafico volume alberi
plot(trees$Volume)
lines(trees$Volume)

describe(cars)



