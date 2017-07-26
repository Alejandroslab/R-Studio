#Dadi
#si creano delle collezioni. spazio campionario, numeri dispari e numeri pari di un dado a 6 facce
#si mettono le virgolette nella dichiarazione per poter inserire i dati come caratteri e non come numeri

spaziocampionario<- c("1","2","3","4","5","6")
EUno<- c("1","3","5")
EDue<- c("2","4","6")
ETre<-c("1","2")

#Per evento unione. Si puo creare una nuova variabile che in questo caso chiameremo unioneE1E2
unioneE1E2<-union(EUno,EDue)

#altrimenti si può visualizzare solo il risultato
union(EUno,EDue)

#per l'intersezione
intersect(EUno,EDue)
#l'intersezione di EUno ed EDue è vuoto

#l'evento complementare di EUno rispetto allo spaziocampionario
setdiff(spaziocampionario,EUno)

#per interrogare R e chiedere se 4 appartiene all'insieme EDue
is.element("4",EDue)

#per generare 3 numeri con criterio casuale nell'intervallo 1-380
sample(1:380, 3)

