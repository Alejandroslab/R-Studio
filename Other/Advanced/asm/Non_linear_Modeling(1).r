# Chapter 7 Lab: Non-linear Modeling

library(ISLR)

# Polynomial Regression and Step Functions

#stima modello polinomiale di grado 4
#comportamento funzione poly
#viene considerata una trasformazione della base x, I(x^2), I(x^3), I(x^4), che porta a elementi ortogonali (polinomi orgogonali)
#i modelli hanno coefficienti diversi ma stesso adattamento e forniscono la stessa previsione, in quanto si stima la proiezione di wage sulla medesima varieta' lineare (rappresentata mediante basi equivalenti)
fit<-lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
model.matrix(fit)
cov(model.matrix(fit))
#base non ortogonale
fit2<-lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
model.matrix(fit2)
#lo stesso modello puo' essere rappresentato come
fit2a<-lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
#oppure
fit2b<-lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)
#creazione griglia di valori in corrispondenza dei quali effettuare le previsioni
agelims<-range(Wage$age)
#valori (lo step di default e' 1)
age.grid<-seq(from=agelims[1],to=agelims[2])
#previsioni (con standard errors)
preds<-predict(fit,newdata=list(age=age.grid),se=TRUE)
#definizione limiti s.e.
se.bands<-cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
#grafico
#par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0)) # per grafico doppio con anche glm
with(Wage,plot(age,wage,xlim=agelims,cex=.5,col="darkgrey"))
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


#controllo equivalenza delle previsioni modello con base polinomiale ortogonale e modello con potenze di age
preds2<-predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

#scelta ordine polinomio (i seguenti modelli sono nested)
fit.1<-lm(wage~age,data=Wage)
fit.2<-lm(wage~poly(age,2),data=Wage)
fit.3<-lm(wage~poly(age,3),data=Wage)
fit.4<-lm(wage~poly(age,4),data=Wage)
fit.5<-lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
#si sceglie polinomio di 3 o 4 grado (marginal rejection)

#dal momento che i polinomi sono ortogonali e' possibile controllo diretto sul p-value dei coefficienti (F=t^2)
coef(summary(fit.5))

#anova ha utilizzo piu' generale, funziona a prescindere dal fatto che i regressori nei modelli siano polinomi ortogonali
fit.1<-lm(wage~education+age,data=Wage)
fit.2<-lm(wage~education+poly(age,2),data=Wage)
fit.3<-lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)

##STIMA GLM (v. corso di Statistica assicurativa)
#fit<-glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
#preds<-predict(fit,newdata=list(age=age.grid),se=T)
#pfit<-exp(preds$fit)/(1+exp(preds$fit))
#se.bands.logit <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
#se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))
#preds<-predict(fit,newdata=list(age=age.grid),type="response",se=T)
#plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
#points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
#lines(age.grid,pfit,lwd=2, col="blue")
#matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


#step function
#function cut per creare factor da variabile quantitativa
table(cut(Wage$age,4))
#stima modello lineare
fit<-lm(wage~cut(age,4),data=Wage)
#coefficienti (stessa interpretazione variabili dummy con corner parameterization)
coef(summary(fit))


# Splines
#la funzione bs nel package spline consente di generare l'intera matrice con le funzioni che definiscono la base, di default si considerano cubic splines
library(splines)
fit<-lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred<-predict(fit,newdata=list(age=age.grid),se=TRUE)
with(Wage,plot(age,wage,col="gray"))
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
#si considerano k=3 nodi -> spline con base composta da 6 funzioni k+3
dim(bs(Wage$age,knots=c(25,40,60)))
#e' possibile specificare direttamente il numero di gradi di libertà (dimensione della base) e i nodi vengono posti in corrispondenza di percentili (di ordine equispaziato)
dim(bs(Wage$age,df=6))
attr(bs(Wage$age,df=6),"knots")
#l'argomento degree (di default 3) consente di specificare la potenza del polinomio utilizzato nella costruzione delle spline

#la funzione ns consente di stimare una natural spline 
fit2<-lm(wage~ns(age,df=4),data=Wage)
#i nodi potrebbero anche essere definiti mediante l'argomento knots
pred2<-predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

#per stimare una smoothing spline si utilizza la funzione smooth.spline

with(Wage,plot(age,wage,xlim=agelims,cex=.5,col="darkgrey"))
title("Smoothing Spline")
fit<-with(Wage,smooth.spline(age,wage,df=16))
#la funzione determina automaticamente il valore di lambda corrispondente a 16 gradi di liberta'

#e' possibile anche avvalersi di cross validation per determinare il livello di smoothness
fit2<-with(Wage,smooth.spline(age,wage,cv=TRUE))
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


#per effettuare una local regression si utilizza la funzione loess
with(Wage,plot(age,wage,xlim=agelims,cex=.5,col="darkgrey"))
title("Local Regression")
#in questo esempio si considerano intorni del punto costituiti dal 20% e dal 50% delle osservazioni
fit<-loess(wage~age,span=.2,data=Wage)
fit2<-loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)




# GAMs

#stima di wage considerando natural splines per year ed age, con presenza anche del predittore categorico education
gam1<-lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
#stima con smoothing splines invece che con natural spline (occorre utilizzare funzione gam nel package GAM)
library(gam)
gam.m3<-gam(wage~s(year,4)+s(age,5)+education,data=Wage)
#crea oggetto gam che puo' essere rappresentato graficamente mediante funzione plot.gam (applicabile anche a oggetto lm)
layout(matrix(1:3,1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.gam(gam1, se=TRUE, col="red")
#la relazione con year sembra lineare; si confrontano 3 modelli: m1 gam che esclude year; m2 gam con funzione lineare di year; m3 gam con funzione spline di year


gam.m1<-gam(wage~s(age,5)+education,data=Wage)
gam.m2<-gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# gam con linear function e' migliore di gam che non include year; non sussiste evidenza della necessita' di considerare una funzione non lineare di year

#riassunto stima del modello
summary(gam.m3)
#previsione
preds<-predict(gam.m2,newdata=Wage)
#si possono utilizzare anche local regression come elementi costitutivi di un gam
gam.lo<-gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")
#e per creare interazioni tra variabili
gam.lo.i<-gam(wage~lo(year,age,span=0.5)+education,data=Wage)
#per rappresentare le superfici di risposta utilizzare package akima
library(akima)
plot(gam.lo.i)
##STIMA modello gam di regressione logistica (v. corso di Statistica assicurativa)
#gam.lr<-gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
#par(mfrow=c(1,3))
#plot(gam.lr,se=T,col="green")
#table(education,I(wage>250))
#gam.lr.s<-gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
#plot(gam.lr.s,se=T,col="green")
