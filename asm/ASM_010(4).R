### R code from vignette source 'ASM_010.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 5: wages_in_Belgium
###################################################
indwages <- read.table(unzip("wages_in_Belgium.zip", "bwages.dat"), header=T)
indwages<-transform(indwages,
  sex=factor(MALE, levels=0:1,labels=c("f","m"))
  )


###################################################
### code chunk number 7: ASM_010.rnw:137-138
###################################################
boxplot(indwages[,1]~indwages$sex,main="Distribuzione dello stipendio rispetto al genere")


###################################################
### code chunk number 10: ASM_010.rnw:154-155
###################################################
boxplot(indwages[,1]~indwages$EDUC,main="Distribuzione dello stipendio rispetto al titolo di studio")


###################################################
### code chunk number 13: ASM_010.rnw:171-172
###################################################
boxplot(indwages[,1]~paste(indwages$sex,indwages$EDUC),main="Distribuzione dello stipendio rispetto al genere e al titolo di studio")


###################################################
### code chunk number 16: ASM_010.rnw:221-240
###################################################
library(mvtnorm)
#n. osservazioni utilizzato per ciascuno dei due gruppi
n<-100
#iniziazione generatore dati (per riproducibilita')
set.seed(123456)
#generazione coordinate primo gruppo di dati (valori variabili X1 e X2)
data1<-rmvnorm(n, mean = c(1,2), sigma = matrix(c(1,0,0,1),2,2))
#assegnazione nomi di riga
rownames(data1)<-paste("A",1:n,sep="")
#generazione coordinate secondo gruppo di dati (valori variabili X1 e X2)
data2<-rmvnorm(n, mean = c(3,1), sigma = matrix(c(1,.5,.5,1),2,2))
#assegnazione nomi di riga
rownames(data2)<-paste("B",1:n,sep="")
#unione in un unico data set
data<-rbind(data1,data2)
#assegnazione nomi alle variabili
colnames(data)<-c("x1","x2")
#assegnazione variabile outcome con categoria A per i punti nel primo gruppo e B per i punti nel secondo gruppo
outcome<-c(rep(c("A","B"),each=n,k=10))


###################################################
### code chunk number 18: ASM_010.rnw:251-261
###################################################
#inizializzazione grafico
plot(data,type="n",xlim=c(-2,6),ylim=c(-2,5))
#rappresentazione punti nel primo gruppo di dati
points(data[1:n,],pch=16,col="blue")
#rappresentazione punti nel secondo gruppo di dati
points(data[1:n+n,],pch=1,col="red")
#creazione griglia come prodotto cartesiano tra gli insiemi indicati
set<-expand.grid(-20:60/10,-20:50/10)
#rappresentazione punti griglia
points(set,pch=1,col="grey",cex=.2)


###################################################
### code chunk number 22: ASM_010.rnw:304-315
###################################################
library(class)
#classificazione con algoritmo KNN (k=3)
knn.pred<-knn(data,set,outcome,k=3)
#creazione nuova figura con punti training set e classificazione punti sulla griglia
plot(data,type="n",xlim=c(-2,6),ylim=c(-2,5))
points(data[1:n,],pch=16,col="blue")
points(data[1:n+n,],pch=1,col="red")
#rappresentazione punti griglia classificati nel primo gruppo
points(set[knn.pred=="A",],pch=16,col="blue",cex=.2)
#rappresentazione punti griglia classificati nel secondo gruppo
points(set[knn.pred=="B",],pch=1,col="red",cex=.2)


###################################################
### code chunk number 26: ASM_010.rnw:343-353
###################################################
#classificazione con algoritmo KNN (k=5)
knn.pred<-knn(data,set,outcome,k=5)
#creazione nuova figura con punti training set e classificazione punti sulla griglia
plot(data,type="n",xlim=c(-2,6),ylim=c(-2,5))
points(data[1:n,],pch=16,col="blue")
points(data[1:n+n,],pch=1,col="red")
#rappresentazione punti griglia classificati nel primo gruppo
points(set[knn.pred=="A",],pch=16,col="blue",cex=.2)
#rappresentazione punti griglia classificati nel secondo gruppo
points(set[knn.pred=="B",],pch=1,col="red",cex=.2)


###################################################
### code chunk number 36: ASM_010.rnw:655-657
###################################################
layout(matrix(1:2,1,2))


###################################################
### code chunk number 37: ASM_010.rnw:659-670
###################################################
with(indwages,plot(WAGE~EXPER,cex=.5))
set<-with(indwages,min(EXPER):max(EXPER))
a<-with(indwages,sapply(set, function(i) mean(WAGE[EXPER==i])))
with(indwages,lines(set,a,col="red",lwd=2))
set<-with(indwages,(min(EXPER)+1):(max(EXPER)-1))
a<-with(indwages,sapply(set, function(i) mean(WAGE[(EXPER>=i-1)&(EXPER<=i+1)])))
with(indwages,lines(set,a,col="blue",lwd=2))
set<-with(indwages,(min(EXPER)+2):(max(EXPER)-2))
a<-with(indwages,sapply(set, function(i) mean(WAGE[(EXPER>=i-2)&(EXPER<=i+2)])))
with(indwages,lines(set,a,col="green",lwd=2))
with(indwages,abline(h=mean(WAGE),col="brown"))


###################################################
### code chunk number 38: ASM_010.rnw:677-688
###################################################
with(indwages,plot(WAGE~EXPER,cex=.5))
set<-with(indwages,min(EXPER):max(EXPER))
a<-with(indwages,sapply(set, function(i) median(WAGE[EXPER==i])))
with(indwages,lines(set,a,col="red",lwd=2))
set<-with(indwages,(min(EXPER)+1):(max(EXPER)-1))
a<-with(indwages,sapply(set, function(i) median(WAGE[(EXPER>=i-1)&(EXPER<=i+1)])))
with(indwages,lines(set,a,col="blue",lwd=2))
set<-with(indwages,(min(EXPER)+2):(max(EXPER)-2))
a<-with(indwages,sapply(set, function(i) median(WAGE[(EXPER>=i-2)&(EXPER<=i+2)])))
with(indwages,lines(set,a,col="green",lwd=2))
with(indwages,abline(h=median(WAGE),col="brown"))


###################################################
### code chunk number 50: ASM_010.rnw:993-1001
###################################################
f1<-function(x) 5-41.667*(x/100)^3+51.875*(x/100)^2-9.125*(x/100)
f2<-function(x) 2+5*((x+40)/100)^2
f3<-function(x) -100*(x/100)^3+150*(x/100)^2-63*(x/100)+12
layout(matrix(1:3,1,3))
curve(f1,xlim=c(0,100),ylim=c(2,13),lwd=2,xlab="x",ylab="y",cex.lab=1.5,main="(a)",cex.main=4)
curve(f2,xlim=c(0,100),ylim=c(2,13),lwd=2,xlab="x",ylab="y",cex.lab=1.5,main="(b)",cex.main=4)
curve(f3,xlim=c(0,100),ylim=c(2,13),lwd=2,xlab="x",ylab="y",cex.lab=1.5,main="(c)",cex.main=4)


###################################################
### code chunk number 58: ASM_010.rnw:1161-1167
###################################################
#relazione teorica
f<-function(x) 5-41.667*(x/100)^3+51.875*(x/100)^2-9.125*(x/100)
#consente di porre piu' grafici nella stessa finestra
layout(matrix(1:2,1,2))
#plot relazione teorica
curve(f,xlim=c(0,100),ylim=c(2,13),lwd=3,xlab="x",ylab="y",cex.lab=1.5)


###################################################
### code chunk number 60: ASM_010.rnw:1172-1187
###################################################
#simulazione dati
set.seed(1234567) #iniziazione generatore dati (per riproducibilita')
x<-runif(50)*100
x<-sort(x)
y<-f(x)+rnorm(50)
#aggiunge scatter plot
points(x,y)
#stima OLS retta
lmest<-lm(y~x)
#output OLS
summary(lmest)
#coefficienti
coef(lmest)
#aggiunge retta al grafico
abline(lmest,col="red")


###################################################
### code chunk number 62: ASM_010.rnw:1192-1203
###################################################
#model matrix
X<-model.matrix(lmest)
#modi alternativi per generare valori stimati dal modello retta
fitted1<-X%*%coef(lmest)
fitted2<-apply(t(X)*coef(lmest),2,sum)
fitted<-lmest$fitted
#modi alternativi per ottenere i residui del modello retta
residuals1<-y-lmest$fitted
residuals<-lmest$res
#training MSE
MSE.training.lm<-mean(lmest$res^2)


###################################################
### code chunk number 64: ASM_010.rnw:1208-1217
###################################################
#generazione del test data set
new.length<-10000
x.new<-runif(new.length)*100
#valori teorici in base a funzione f+errore
y.new<-f(x.new)+rnorm(new.length)
#valori previsti dal modello retta
y.new.pred<-predict(lmest,data.frame(x=x.new))
#test MSE
MSE.test.lm<-mean((y.new-y.new.pred)^2)


###################################################
### code chunk number 66: ASM_010.rnw:1222-1239
###################################################
#funzione per calcolo training e test MSE per smoothing splines in funzione dei gradi di liberta'

#le variabili x e y contengono i dati e figurano nel general environment (sono free variables per sm.funct)
sm.funct<-function(dof) {
  fit<-smooth.spline(x,y,df=dof)
  pred<-predict(fit,x)
  MSE.training<-mean((y-pred$y)^2)
  pred.new<-predict(fit,x.new)
  MSE.test<-mean((y.new-pred.new$y)^2)
  return(c(MSE.training,MSE.test))
}
#calcolo training e test MSE in corrispondenza dei gradi di liberta' 2:40
evaluation<-sapply(2:40,sm.funct)
#evaluation[1,] contiene i valori training MSE
#evaluation[2,] contiene i valori test MSE
#determinazione gradi di liberta' modello spline con minimo MSE.test
df<-which.min(evaluation[2,])+1


###################################################
### code chunk number 68: ASM_010.rnw:1244-1263
###################################################
#stima modello spline 'ottimo'
fit2<-smooth.spline(x,y,df=df)
#valori previsti dal modello smoothing spline in corrispondenza del training data set
pred2<-predict(fit2,x)
#rappresentazione grafica modello smoothing spline
lines(pred2$x,pred2$y,col="blue")
#training MSE modello smoothing spline
MSE.training.2<-mean((y-pred2$y)^2)
#valori previsti dal modello smoothing spline
pred2.new<-predict(fit2,x.new)
#test MSE modello smoothing spline
MSE.test.2<-mean((y.new-pred2.new$y)^2)
#secondo modello smoothing spline
fit3<-smooth.spline(x,y,df=35)
pred3<-predict(fit3,x)
lines(pred3$x,pred3$y,col="green")
MSE.training.3<-mean((y-pred3$y)^2)
pred3.new<-predict(fit3,x.new)
MSE.test.3<-mean((y.new-pred3.new$y)^2)


###################################################
### code chunk number 70: ASM_010.rnw:1268-1279
###################################################
#rappresentazione grafica MSE.training e MSE.test su nuovo grafico (plot e' comando grafico di tipo high level, quindi procede su nuovo valore layout)
plot(2:40,evaluation[1,],type="l",ylim=c(0,ceiling(max(evaluation))),xlab="flessibilita'",ylab="MSE",xaxt="n",yaxt="n",bty="n",cex.lab=1.5)
axis(1,pos=0,cex=2,at=0:40)
axis(2,pos=0,cex=2)
lines(2:40,evaluation[2,])
#evidenziazione training e test MSE per i 3 modelli stimati
points(c(2,2),evaluation[,1],col="red",pch=15,cex=2)
points(c(df,df),evaluation[,df-1],col="blue",pch=15,cex=2)
points(c(35,35),evaluation[,34],col="green",pch=15,cex=2)
#retta con valore variabilit\`a irriducibile
abline(h=1,lty=2)


###################################################
### code chunk number 73: ASM_010.rnw:1295-1304
###################################################
ff<-function(f) {
  layout(matrix(1:2,1,2))
  curve(f,xlim=c(0,100),ylim=c(2,13),lwd=3,xlab="x",ylab="y",cex.lab=1.5)
  set.seed(1234567)
  ...
  ...
  ...
  abline(h=1,lty=2)
}


###################################################
### code chunk number 74: ASM_010.rnw:1311-1361
###################################################
ff<-function(f) {
  layout(matrix(1:2,1,2))
  curve(f,xlim=c(0,100),ylim=c(2,13),lwd=3,xlab="x",ylab="y",cex.lab=1.5)
  set.seed(1234567)
  x<-runif(50)*100
  x<-sort(x)
  y<-f(x)+rnorm(50)
  points(x,y)
  lmest<-lm(y~x)
  summary(lmest)
  coef(lmest)
  abline(lmest,col="red")
  residuals<-lmest$res
  (MSE.training.lm<-mean(lmest$res^2))
  new.length<-10000
  x.new<-runif(new.length)*100
  y.new<-f(x.new)+rnorm(new.length)
  y.new.pred<-predict(lmest,data.frame(x=x.new))
  (MSE.test.lm<-mean((y.new-y.new.pred)^2))
  sm.funct<-function(dof) {
    fit<-smooth.spline(x,y,df=dof)
    pred<-predict(fit,x)
    MSE.training<-mean((y-pred$y)^2)
    pred.new<-predict(fit,x.new)
    MSE.test<-mean((y.new-pred.new$y)^2)
    return(c(MSE.training,MSE.test))
  }
  evaluation<-sapply(2:40,sm.funct)
  df<-which.min(evaluation[2,])+1
  fit2<-smooth.spline(x,y,df=df)
  pred2<-predict(fit2,x)
  lines(pred2$x,pred2$y,col="blue")
  (MSE.training.2<-mean((y-pred2$y)^2))
  pred2.new<-predict(fit2,x.new)
  (MSE.test.2<-mean((y.new-pred2.new$y)^2))
  fit3<-smooth.spline(x,y,df=35)
  pred3<-predict(fit3,x)
  lines(pred3$x,pred3$y,col="green")
  (MSE.training.3<-mean((y-pred3$y)^2))
  pred3.new<-predict(fit3,x.new)
  (MSE.test.3<-mean((y.new-pred3.new$y)^2))
  plot(2:40,evaluation[1,],type="l",ylim=c(0,ceiling(max(evaluation))),xlab="flessibilita'",ylab="MSE",xaxt="n",yaxt="n",bty="n",cex.lab=1.5)
  axis(1,pos=0,cex=2,at=0:40)
  axis(2,pos=0,cex=2)
  lines(2:40,evaluation[2,])
  points(c(2,2),evaluation[,1],col="red",pch=15,cex=2)
  points(c(df,df),evaluation[,df-1],col="blue",pch=15,cex=2)
  points(c(35,35),evaluation[,34],col="green",pch=15,cex=2)
  abline(h=1,lty=2)
}


###################################################
### code chunk number 76: ASM_010.rnw:1367-1369
###################################################
f2<-function(x) 2+5*((x+40)/100)^2
ff(f2)


###################################################
### code chunk number 80: ASM_010.rnw:1380-1382
###################################################
f3<-function(x) -100*(x/100)^3+150*(x/100)^2-63*(x/100)+12
ff(f3)


###################################################
### code chunk number 86: ASM_010.rnw:1482-1522
###################################################
decomposition<-function(f) {
  set.seed(1234)
  testxobs<-1:100
  testyobs<-f(testxobs)+rnorm(100)
  decomposition.ols<-function(f) {
    x<-runif(50)*100
    y<-f(x)+rnorm(50)
    lmest<-lm(y~x)
    predict(lmest,data.frame(x=testxobs))
  }
  a<-replicate(1000, decomposition.ols(f))
  a.lin<-c(mean((f(testxobs)-apply(a,1,mean))^2),
    mean(apply(a,1,var))
    )
  
  
  decomposition.sm<-function(dof,f) {
    x<-runif(50)*100
    y<-f(x)+rnorm(50)
    fit<-smooth.spline(x,y,df=dof)
    predict(fit,testxobs)$y
  }
  a.sm<-sapply(3:40,function(dof) {
    set.seed(1234)
    testxobs<-1:100
    testyobs<-f(testxobs)+rnorm(100)
    a<-replicate(1000, decomposition.sm(dof,f))
    c(mean((f(testxobs)-apply(a,1,mean))^2),
    mean(apply(a,1,var))
    )
    }
    )
  decomp<-cbind(a.lin,a.sm)
  plot(2:40,decomp[1,],type="l",ylim=c(0,ceiling(max(decomp))),xlab="flessibilita'",ylab="",xaxt="n",yaxt="n",bty="n",cex.lab=3,col="blue")
  axis(1,pos=0,cex=3,at=0:40)
  axis(2,pos=0,cex=3)
  lines(2:40,decomp[2,],col="red")
  lines(2:40,1+apply(decomp,2,sum),lwd=3)
  abline(h=1,lty=2)
}


###################################################
### code chunk number 88: ASM_010.rnw:1532-1536
###################################################
layout(matrix(1:3,1,3))
decomposition(f1)
decomposition(f2)
decomposition(f3)


###################################################
### code chunk number 110: ASM_010.rnw:1938-1947
###################################################
names(indwages)
#trasformazione della variabile EDUC in fattore
indwages<-transform(indwages,
  educf=factor(EDUC)
  )
#1 modello
summary(output1<-lm(WAGE~sex+educf+EXPER,data=indwages))
layout(matrix(1:6,2,3))
plot(output1,which=1:6)


###################################################
### code chunk number 113: ASM_010.rnw:1968-1978
###################################################
summary(output2<-lm(WAGE~sex+educf+poly(EXPER,2,raw=TRUE),data=indwages))
layout(matrix(1:6,2,3))
plot(output2,which=1:6)
#heteroscedasticity and deviation from normality
#a lot of possible both outliers and leverage data, some influential data:
#compare leverages with the value
crit<-2*length(output2$coef)/nrow(indwages)
sum(hatvalues(output2)>crit)
#no multicollinearity issues
car::vif(output2)


###################################################
### code chunk number 116: ASM_010.rnw:1999-2002
###################################################
#heteroscedasticity issue is evident also from studentised residuals 
plot(rstudent(output2)~predict(output2))
#identify(rstudent(output2)~predict(output2))


###################################################
### code chunk number 119: ASM_010.rnw:2023-2029
###################################################
#possible solution: logarithmic transformation of the dependent variable
summary(output3<-lm(LNWAGE~sex+educf+poly(EXPER,4,raw=TRUE),data=indwages))
layout(matrix(1:6,2,3))
plot(output3,which=1:6)
#check for multicollinearity issues
car::vif(output3)


###################################################
### code chunk number 122: ASM_010.rnw:2050-2057
###################################################
summary(output4<-lm(LNWAGE~sex+educf+poly(EXPER,3,raw=TRUE),data=indwages))
summary(output4<-lm(LNWAGE~sex+educf+poly(EXPER,2,raw=TRUE),data=indwages))
layout(matrix(1:6,2,3))
plot(output4,which=1:6)
#check for multicollinearity issues
car::vif(output4)
#heteroscedasticity seems to be solved


###################################################
### code chunk number 124: ASM_010.rnw:2075-2092
###################################################
drop<-c(264,312,677)
indwages[drop,]
summary(output4<-lm(LNWAGE~sex+educf+poly(EXPER,4,raw=TRUE),data=indwages,subset=(1:nrow(indwages))[-drop]))
layout(matrix(1:6,2,3))
plot(output4,which=1:6)
drop<-c(drop,510,816,1166,462,1300)
indwages[drop,]
summary(output4<-lm(LNWAGE~sex+educf+poly(EXPER,4,raw=TRUE),data=indwages,subset=(1:nrow(indwages))[-drop]))
layout(matrix(1:6,2,3))
plot(output4,which=1:6)
drop<-c(drop,340)
indwages[drop,]
summary(output4<-lm(LNWAGE~sex+educf+poly(EXPER,4,raw=TRUE),data=indwages,subset=(1:nrow(indwages))[-drop]))
layout(matrix(1:6,2,3))
plot(output4,which=1:6)
#check for multicollinearity issues
car::vif(output4)


###################################################
### code chunk number 126: ASM_010.rnw:2102-2111
###################################################
#ridefiniamo indwages togliendo i valori influenti e cerchiamo di determinare l'ordine ottimo del polinomio in EXPER
indwages<-indwages[-drop,]
#is the power in poly(EXPER) correct? 3 seems to be a better choice
summary(output5<-lm(LNWAGE~sex+educf+poly(EXPER,4,raw=TRUE),data=indwages))
#training MSE
with(indwages, mean((LNWAGE-predict(output5))^2))
summary(output5<-lm(LNWAGE~sex+educf+poly(EXPER,3,raw=TRUE),data=indwages))
#training MSE
with(indwages, mean((LNWAGE-predict(output5))^2))


###################################################
### code chunk number 128: ASM_010.rnw:2121-2128
###################################################
#model.i funzione che stima modello con polinomio di ordine i per EXPER
model.i<-function(i) lm(LNWAGE~sex+educf+poly(EXPER,i,raw=TRUE),data=indwages)

(n<-nrow(indwages))
#andamento TRAINING MSE (decrescente rispetto a ordine polinomio)
a<-sapply(1:10, function(i) with(indwages, mean((LNWAGE-predict(model.i(i)))^2)))
plot(1:10,a,type="l")


###################################################
### code chunk number 131: ASM_010.rnw:2149-2160
###################################################
#validation set
validation.set<-function(formula) {
  train<<-sample(n,n/2)
  val.output5<-lm(formula,data=indwages,subset=train)
  with(indwages, mean((LNWAGE-predict(val.output5,indwages))[-train]^2))
}

a<-sapply(1:10, function(i) validation.set(LNWAGE~sex+educf+poly(EXPER,i,raw=TRUE)))
plot(1:10,a,type="l")

table(replicate(1000,which.min(sapply(1:10, function(i) validation.set(LNWAGE~sex+educf+poly(EXPER,i,raw=TRUE))))))


###################################################
### code chunk number 134: ASM_010.rnw:2181-2189
###################################################
#LOOCV
library(boot)
cv.error<-function(i) {
  glm.fit1<-glm(LNWAGE~sex+educf+poly(EXPER,i,raw=TRUE),data=indwages)
  cv.glm(indwages,glm.fit1)$delta[1]
  }
(a<-sapply(1:10, function(i) cv.error(i)))
plot(1:10,a,type="l")


###################################################
### code chunk number 137: ASM_010.rnw:2210-2219
###################################################
#k-Fold CV
library(boot)
cv.error<-function(i) {
  glm.fit1<-glm(LNWAGE~sex+educf+poly(EXPER,i,raw=TRUE),data=indwages)
  cv.glm(indwages,glm.fit1,K=10)$delta[1]
  }

(a<-sapply(1:10, function(i) cv.error(i)))
plot(1:10,a,type="l")


###################################################
### code chunk number 140: ASM_010.rnw:2240-2242
###################################################
(a<-replicate(20,sapply(1:10, function(i) cv.error(i))))
boxplot(t(a))


###################################################
### code chunk number 143: ASM_010.rnw:2263-2296
###################################################
boot.fn<-function(data,index) return(coef(lm(LNWAGE~sex+educf+poly(EXPER,3,raw=TRUE),data=data,subset=index)))
boot.fn(indwages,1:n)

set.seed(123)
boot.fn(indwages,sample(n,n,replace=TRUE))

(a<-boot(indwages,boot.fn,1000))
summary(output5<-lm(LNWAGE~sex+educf+poly(EXPER,3,raw=TRUE),data=indwages))
layout(matrix(1:9,3,3))
anames<-names(coef(output5))
sapply(1:9, function(i) hist(a$t[,i],main="",xlab=anames[i],freq=FALSE))


summary(indwages$WAGE)
hist(indwages$WAGE)
library(rriskDistributions)
fit.cont(indwages$WAGE)
fit.cont(log(indwages$WAGE))
t.test(indwages$WAGE)

boot.fn<-function(data,index) return(mean(data[index]))
boot.fn(indwages$WAGE,1:n)

set.seed(123)
boot.fn(indwages$WAGE,sample(n,n,replace=TRUE))

(a<-boot(indwages$WAGE,boot.fn,1000))
quantile(a$t,c(.025,.975))
t.test(indwages$WAGE)
hist(a$t)
library(rriskDistributions)
fit.cont(c(a$t))


