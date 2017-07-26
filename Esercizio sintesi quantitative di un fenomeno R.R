#rappresentazione dataframe Meloni
#creiamo meloniDF come nuovo data frame
#R è case sensitive

meloniDF<-data.frame(
x= c(0,2,0,1,2,2),
y= c(1.9,1.8,1.9,1.7,2.1,2.1),
z= c(0,1,0,1,1,1))

#x,y,z sono variabili
#le unità statistiche sono 6.
#y indica il peso ipotetico dei meloni

rm(z)  #comando per rimuovere variabile z dall'environment

meloniDF #richiamare meloniDF in console
meloniDF$x #il simbolo $ è utilizzato per richiamare la colonna
#associata alla variabile x

meloniDF$z[5] #richiamare il valore della variabile z in corrispondenza
#dell'indice 5

sort(meloniDF$y) #valori ordinati
unique(meloniDF$y) #valori distinti ordinati

table(meloniDF$y) #distribuzione di frequenze assolute
table(meloniDF$y)/length(meloniDF$y) #distribuzione di frequenze relative

cumsum(table(meloniDF$y)) #distribuzione di frequenze assolute cumulate (è una sommatoria)
cumsum(table(meloniDF$y))/length(meloniDF$y) #distribuzione di frequenze relative cumulate

mean(meloniDF$y) #media aritmetica

plot(mean(meloniDF$y),col='red') #disegna grafico della media aritmetica (in questo caso un punto)
#con colore rosso

AmpiezzaCampoVariazione <- max(meloniDF$y)-min(meloniDF$y)
#il campo di variazione è una semplice sintesi di variabilità che si 
#calcola facendo la differenza tra i valori estremi della variabile

#per la varianza occorre prima dimensionare una variabile fattore correzione
fattoreCorrezione<-(length(meloniDF$y)-1)/length(meloniDF$y)
#la varianza sarà
varianza<-var(meloniDF$y)*fattoreCorrezione

#coefficiente di variazione percentuale (rapporto tra deviazione standard S e media)
coeffVariazionePerc<-sqrt(varianza)/mean(meloniDF$y)*100

#per ottenere il terzo quantile di una distribuzione (quantile in questo caso è una parola chiave del sistema)
quantile(meloniDF$y, probs=0.75, type=1)

#per ottenere informazioni e aiuto sull'utilizzo della sintassi si antepone ?
?quantile

#per ottenere alcuni importanti quantili con una istruzione
#minimo, primo quartile, mediana, terzo quartile e massimo
quantile(meloniDF$y, probs = c(0.1,0.25,0.5,0.75,0.9),type=1)

#per ottenere i quantili più importanti e la media aritmetica in modo piu semplice
summary(meloniDF$y)

#per ottenere grafico di distribuzione empirica 
plot(ecdf(meloniDF$y))

#istogramma
hist()
#diagramma a scatola con baffi
boxplot()
#diagramma di frequenze a barre verticali
barplot()
#diagramma a settori circolari
pie()
#esempio. si dimensiona una variabile in cui si inserisce una distribuzione di frequenze assolute
distriFre<-table(meloniDF$y)
#dopodichè si disegna il diagramma a settore circolare o torta richiamando la variab appena creata
pie(distriFre)





