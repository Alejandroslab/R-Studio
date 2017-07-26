#Inferenza statistica, single proportion: hypothesis test and confidence interval 

#proportion test  
prop.test(98,162)

#one tailed test con il 90% di confidenza 
prop.test(98,162, alt="greater", conf.level= .90)


#le prime 5 linee della collezione data 'quakes'. Lo spazio vuoto indica di visualizzare tutte le variabili
quakes[1:5, ]

#carica la variabile magnitudo di quakes in una variabile denominata mag
mag<-quakes$mag

#primi 5 dati di magnitudo
mag[1:5]

#t-test
t.test(mag)

#one-sided t-test with w/mu=4
t.test(mag,alternative="greater", mu=4)
