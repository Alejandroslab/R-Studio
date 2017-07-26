#simulazione lancio di una moneta. al crescere del numero di prove la frequenza
#relativa di testa, qui identificata con 0, tende a stabilizzarsi attorno a 0.5

n=500
#generatore di n numeri casuali nell'intervallo 0,1 con prob 0.5
numcasuali=sample(0:1,n,replace=TRUE, prob = c(0.5,0.5))

f=cumsum(numcasuali)/(1:n)

plot(1:n,f,type="l",xlab = "Numero delle prove", ylab="Frequenza relativa di successo")
#linea con altezza pari a 0.5
abline(h=0.5,lty=2,col=2)
