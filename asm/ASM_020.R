### R code from vignette source 'ASM_020.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 5: ASM_020.rnw:145-152
###################################################
x1<-c(-2,1,1)
x2<-c(0,-1,1)
cor(x1,x2) # regressori non correlati
f<-function() {
y<-30+5*x1+10*x2+rnorm(3,1)
c(coef(lm(y~x1+x2)))
}


###################################################
### code chunk number 6: ASM_020.rnw:160-165
###################################################
#simulazione modello con 1000 replicazioni
set.seed(123)
a<-replicate(1000,f())
layout(matrix(1:3,1,3))
sapply(1:3,function(i) hist(a[i,],main="",xlab=rownames(a)[i],freq=FALSE))


###################################################
### code chunk number 7: ASM_020.rnw:167-174
###################################################
layout(matrix(1:3,1,3))
hist(a[1,],main="",xlab=rownames(a)[1],freq=FALSE,xlim=c(28,34),ylim=0:1)
hist(a[2,],main="",xlab=rownames(a)[2],freq=FALSE,xlim=c(2,8),ylim=0:1)
hist(a[3,],main="",xlab=rownames(a)[3],freq=FALSE,xlim=c(7,13),ylim=0:1)


###################################################
### code chunk number 8: ASM_020.rnw:184-195
###################################################
#calcolo varianza teorica dello stimatore OLS 

#sigma_E era stato posto pari a 1
X<-cbind(1,x1,x2)
diag(solve(t(X)%*%X))
#calcolo varianza stime
apply(a,1,var)
#verifica che i residui sono nulli
y<-30+5*x1+10*x2+rnorm(3,1)
residuals(out<-lm(y~x1+x2))
summary(out)


###################################################
### code chunk number 19: ASM_020.rnw:712-722
###################################################
#lettura dati
credit <- read.table("credit.csv",sep=",",header=T)
#nomi variabili
names(credit)
#la prima variabile contiene gli identificativi u.s.
credit<-credit[,-1]
#numero u.s.
n<-nrow(credit)
#check su not available
max(is.na(credit))


###################################################
### code chunk number 21: ASM_020.rnw:732-734
###################################################
#scatter plots (senza variabili categoriche)
plot(credit[,-(7:10)])


###################################################
### code chunk number 24: ASM_020.rnw:750-758
###################################################
#modello completo
output<-lm(Balance~.,data=credit)
matr.dati<-model.matrix(output)
summary(output)
#modello con variabili esplicative standardizzate
matr.dati[,2:7]<-scale(matr.dati[,2:7])
summary(lm(credit$Balance~0+matr.dati))
#si considera nel seguito il modello con le variabili in scala originaria


###################################################
### code chunk number 26: ASM_020.rnw:768-772
###################################################
#controllo multicollinearita'
library(car)
vif(output)
#Limit e Rating sono fortemente correlate


###################################################
### code chunk number 27: ASM_020.rnw:779-793
###################################################
#matrice dei dati (con colonna unitaria)
X<-model.matrix(output)
#numero variabili nel modello completo (comprese le dummy di ricodifica variabili categoriche)
p<-ncol(X)-1
#package sampling, contiene funzione writesample che estrae elementi combinazioni \choose(n,k)
set.seed(123)
library(sampling)
#variabili selezionate nei k step della best subset selection (lista con p elementi)
a<-lapply(1:p, function(i) writesample(i,p))
#identificazione degli indici delle variabili selezionate nei k step della best subset selection (lista con p elementi)
a<-lapply(1:(p-1),function(i) t(apply(a[[i]],1,function(x) which(x==1))))
a[[1]]<-matrix(1:p,p,1)
a[[p]]<-matrix(1:p,1,p)
#manca solo null model


###################################################
### code chunk number 28: ASM_020.rnw:800-809
###################################################
Y<-credit$Balance
#funzione che calcola RSS e Rsq per tutti i modelli
bestsubset0<-function(k,j) {
  Xtmp<-X[,c(1,a[[k]][j,]+1)]
  betatmp<-solve(t(Xtmp)%*%Xtmp,t(Xtmp)%*%Y)
  Yhat<-Xtmp%*%betatmp
  c(RSStmp<-sum((Y-Yhat)^2),Rsqtmp<-cor(Y,Yhat)^2)
}
bestsubsetstats<-lapply(1:p,function(k) sapply(1:nrow(a[[k]]),function(j) bestsubset0(k,j)))


###################################################
### code chunk number 30: ASM_020.rnw:819-827
###################################################
#grafico RSS
#null model
RSS<-(n-1)*var(Y)
layout(matrix(1:2,1,2))
plot(0,RSS,xlim=c(0,11),ylim=c(0,RSS),xlab="Numero di variabili esplicative",ylab="Residual Sum of Squares")
graphtmp<-sapply(1:p,function(k) points(rep(k,ncol(bestsubsetstats[[k]])),bestsubsetstats[[k]][1,]))
points(0:p,c(RSS,sapply(1:p,function(k) min(bestsubsetstats[[k]][1,]))),pch=16,col="red")
lines(0:p,c(RSS,sapply(1:p,function(k) min(bestsubsetstats[[k]][1,]))),col="red")


###################################################
### code chunk number 31: ASM_020.rnw:834-841
###################################################
#grafico Rsq
#null model
Rsq<-0
plot(0,Rsq,xlim=c(0,11),ylim=c(0,1),xlab="Numero di variabili esplicative",ylab=expression(R^2))
graphtmp<-sapply(1:p,function(k) points(rep(k,ncol(bestsubsetstats[[k]])),bestsubsetstats[[k]][2,]))
points(0:p,c(Rsq,sapply(1:p,function(k) max(bestsubsetstats[[k]][2,]))),pch=16,col="red")
lines(0:p,c(Rsq,sapply(1:p,function(k) max(bestsubsetstats[[k]][2,]))),col="red")


###################################################
### code chunk number 34: ASM_020.rnw:862-871
###################################################
#esiste funzione regsubsets nel package leaps che restituisce modello migliore con 0:p regressori in funzione di diversi criteri: c("r2","adjr2","Cp","bic")
library(leaps)
#sintassi regsubsets simile a lm (nvmax=11 per considerare tutti i modelli)
regfit.full<-regsubsets(Balance~.,data=credit,nvmax=11)
#summary flag "*" per variabili inserite nei modelli 
(regfit.full.summary<-summary(regfit.full))
names(regfit.full.summary)
#per estrarre i rsq associati ai modelli selezionati con n. variabili 1:p (lunghezza vettore = p)
regfit.full.summary$rsq


###################################################
### code chunk number 37: ASM_020.rnw:884-897
###################################################
layout(matrix(1:4,2,2,byrow=TRUE))
plot(regfit.full.summary$rss/n,xlab="N. variabili",ylab="MSE",type="l")
a<-which.min(regfit.full.summary$rss)
points(a,regfit.full.summary$rss[a]/n,cex=5,pch=20)
plot(regfit.full.summary$adjr2,xlab="N. variabili",ylab="Adjusted RSq",type="l")
a<-which.max(regfit.full.summary$adjr2)
points(a,regfit.full.summary$adjr2[a],col="red",cex=2,pch=20)
plot(regfit.full.summary$cp,xlab="N. variabili",ylab=expression(C[p]),type="l")
a<-which.min(regfit.full.summary$cp)
points(a,regfit.full.summary$cp[a],col="red",cex=2,pch=20)
plot(regfit.full.summary$bic,xlab="N. variabili",ylab="BIC",type="l")
a<-which.min(regfit.full.summary$bic)
points(a,regfit.full.summary$bic[a],col="red",cex=2,pch=20)


###################################################
### code chunk number 40: ASM_020.rnw:918-920
###################################################
layout(matrix(1:4,2,2,byrow=TRUE))
graphtmp<-sapply(c("r2","adjr2","Cp","bic"), function(i) plot(regfit.full,scale=i))


###################################################
### code chunk number 42: ASM_020.rnw:933-953
###################################################
#scelta del modello migliore tramite validation set e cross validation
#in questo caso la fase preliminare di selezione deve essere effettuata sul training data set

#validation set
quota.training<-.75
train<-sample(n,round(n*quota.training,0))
regfit.best<-regsubsets(Balance~.,data=credit[train,],nvmax=11)

#funzione che calcola MSE.test
MSE.test<-function(obj=regfit.best,k,train=train) {
  coef.k<-coef(regfit.best,k)
  espl.k<-names(coef.k)
  test.matrix<-model.matrix(Balance~.,data=credit[-train,])
  pred.k<-test.matrix[,espl.k]%*%coef.k
  mean((Y[-train]-pred.k)^2)
}

(a<-sapply(1:p,function(k) MSE.test(obj=regfit.best,k,train)))
which.min(a)
coef(regfit.best,which.min(a))


###################################################
### code chunk number 43: ASM_020.rnw:960-971
###################################################
#funzione per validation set
val.set<-function(k) {
  train<-sample(n,round(n*quota.training,0))
  regfit.best<-regsubsets(Balance~.,data=credit[train,],nvmax=11)
  MSE.test(obj=regfit.best,k,train)
}

(a<-sapply(1:p,function(k) val.set(k)))
which.min(a)

(fig6.3vs<-sapply(1:p,function(k) val.set(k)^.5))


###################################################
### code chunk number 44: ASM_020.rnw:978-986
###################################################
#funzione predict per classe regsubsets
predict.regsubsets<-function(object,form=as.formula(Balance~.),newdata,k) {
#  form<-as.formula(object$call[[2]]) # eliminata in quanto l'oggetto cambia classe se utilizzato all'interno di una funzione
  test.matrix<-model.matrix(form,newdata)
  coef.k<-coef(object,k)
  espl.k<-names(coef.k)
  test.matrix[,espl.k]%*%coef.k
}


###################################################
### code chunk number 45: ASM_020.rnw:993-1005
###################################################
#funzione alternativa per validation set
val.set<-function(k) {
  train<-sample(n,round(n*quota.training,0))
  regfit.best<-regsubsets(Balance~.,data=credit[train,],nvmax=11)
  newdata<-credit[-train,]
  mean((credit$Balance[-train]-predict(object=regfit.best,form=Balance~.,newdata,k))^2)
}

(a<-sapply(1:p,function(k) val.set(k=k)))
which.min(a)

fig6.3.vs<-sapply(1:p,function(k) val.set(k=k)^.5)


###################################################
### code chunk number 46: ASM_020.rnw:1012-1018
###################################################
#occorre ripetere best subset con quella dimensione
regfit.best<-regsubsets(Balance~.,data=credit[train,],nvmax=11)
coef(regfit.best,which.min(a))

#attenzione ricordarsi della possibile variabilita' dei risultati della procedura validation set
table(replicate(1000,which.min(sapply(1:p,function(k) val.set(k=k)))))


###################################################
### code chunk number 47: ASM_020.rnw:1025-1032
###################################################
#k fold cv
k<-10
set.seed(123)
#JWHT
folds<-sample(1:k,n,replace=TRUE)
#se n e' multiplo di k, meglio con
folds<-rep(1:k,each=n/k)[sample(n)]


###################################################
### code chunk number 48: ASM_020.rnw:1039-1046
###################################################
a<-matrix(NA,k,p)
for(j in 1:k) {
  best.fit<-regsubsets(Balance~.,data=credit[folds!=j,],nvmax=11)
  for(i in 1:p) a[j,i]<-mean((credit$Balance[folds==j]-predict(object=best.fit,newdata=credit[folds==j,],k=i))^2)
  }
mean.cv.errors<-apply(a,2,mean)
which.min(mean.cv.errors)


###################################################
### code chunk number 49: ASM_020.rnw:1053-1056
###################################################
#occorre ripetere best subset con quella dimensione
regfit.best<-regsubsets(Balance~.,data=credit[train,],nvmax=11)
coef(regfit.best,which.min(mean.cv.errors))


###################################################
### code chunk number 51: ASM_020.rnw:1066-1076
###################################################
layout(matrix(1:3,1,3))
plot(summary(regfit.best)$bic,type="l")
points(1:p,summary(regfit.best)$bic,type="b") # controllare quale trasformazione di bic hanno effettuato JWHT
plot(fig6.3.vs,type="l")
points(1:p,fig6.3.vs)
plot(mean.cv.errors^.5,type="l")
points(1:p,mean.cv.errors^.5)
sd.cv.errors<-sd(mean.cv.errors^.5)
#limite per la one standard-error rule
abline(h=min(mean.cv.errors^.5)+sd.cv.errors)


###################################################
### code chunk number 54: ASM_020.rnw:1097-1100
###################################################
#forward selection
regfit.fwd<-regsubsets(Balance~.,data=credit,nvmax=11,method="forward")
summary(regfit.fwd)


###################################################
### code chunk number 57: ASM_020.rnw:1113-1116
###################################################
#backward selection
regfit.bwd<-regsubsets(Balance~.,data=credit,nvmax=11,method="backward")
summary(regfit.bwd)


###################################################
### code chunk number 59: ASM_020.rnw:1126-1130
###################################################
#nel caso in esame i tre metodi restituiscono modello con le stesse variabili
coef(regfit.full,6)
coef(regfit.fwd,6)
coef(regfit.bwd,6)


###################################################
### code chunk number 60: ASM_020.rnw:1137-1149
###################################################
#Limit e Rating sono fortemente correlate,
#Provare analisi con sostituzione con nuova variabile media delle due std
credit<-transform(credit,
  Average.Worthiness=((Limit-mean(Limit))/sd(Limit)+(Rating-mean(Rating))/sd(Rating))/2
  )
names(credit)
#eliminazione Limit e Rating
credit<-credit[,-(2:3)]
#nuova regressione
output<-lm(Balance~.,data=credit)
summary(output)
vif(output)


###################################################
### code chunk number 75: ASM_020.rnw:1606-1617
###################################################
#lettura dati
credit <- read.table("credit.csv",sep=",",header=T)
names(credit)
#la prima variabile contiene gli identificativi u.s.
credit<-credit[,-1]
#numero u.s.
n<-nrow(credit)
#estrazione della model.matrix relativa a regressione completa
x<-model.matrix(Balance~.,data=credit)[,-1]
#variabile dipendente
y<-credit$Balance


###################################################
### code chunk number 76: ASM_020.rnw:1624-1634
###################################################
#Ridge Regression
library(glmnet)
#griglia valori per parametro tuning \lambda
grid<-c(10^seq(from=10,to=-2,length=100),0)
#funzione glmnet: argomento alpha=0 --> rigde, alpha=1 --> lasso
ridge.mod<-glmnet(x,y,alpha=0,lambda=grid)
#le variabili sono di default standardizzate (standardize=TRUE) 
head(ridge.mod$lambda)
tail(ridge.mod$lambda)
dim(coef(ridge.mod))


###################################################
### code chunk number 77: ASM_020.rnw:1641-1652
###################################################
#coefficienti per lambda in 50 posizione nel vettore grid
grid[50]
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(coef(ridge.mod)[-1,50]^2)^.5
#coefficienti per lambda in 60 posizione nel vettore grid
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(coef(ridge.mod)[-1,60]^2)^.5


###################################################
### code chunk number 78: ASM_020.rnw:1659-1668
###################################################
#coefficienti per lambda=0 in posizione 101 nel vettore grid
ridge.mod$lambda[101]
coef(ridge.mod)[,101]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(coef(ridge.mod)[-1,101]^2)^.5
#stima OLS
coef(lm(y~x))
#per ottenere valori precisi con glmnet occorre aumentare il parametro di precisione dell'algoritmo iterativo
coef(glmnet(x,y,alpha=0,lambda=grid,thresh=1e-25))[,101]


###################################################
### code chunk number 79: ASM_020.rnw:1675-1688
###################################################
#coefficienti per lambda=10^10 in posizione 1 nel vettore grid
ridge.mod$lambda[1]
coef(ridge.mod)[,1]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(coef(ridge.mod)[-1,1]^2)^.5
#stima OLS modello costante
coef(lm(y~1))
mean(y)
#per ottenere valori precisi con glmnet occorre aumentare il parametro di precisione dell'algoritmo iterativo
coef(glmnet(x,y,alpha=0,lambda=grid,thresh=1e-25))[,1]

#previsione (s nuovo valore di lambda
predict(ridge.mod,s=50,type="coefficients")[1:12,]


###################################################
### code chunk number 80: ASM_020.rnw:1695-1706
###################################################
#stima MSE_test
#definizione indici per training data set
set.seed(1234)
train<-sample(n,n/2)
y.test<-y[-train]
#stima modello ridge regression su training data set
ridge.mod<-glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-25)
#calcolo previsioni in corrispondenza di lambda =4 newx matrice con osservazioni test data set
ridge.pred<-predict(ridge.mod,newx=x[-train,],s=4)
#stima MSE_test
mean((y.test-ridge.pred)^2)


###################################################
### code chunk number 81: ASM_020.rnw:1713-1718
###################################################
#calcolo previsioni in corrispondenza di lambda =10^10 newx matrice con osservazioni test data set
ridge.pred<-predict(ridge.mod,newx=x[-train,],s=1e10)
#stima MSE_test
mean((y.test-ridge.pred)^2)
var(y[-train])


###################################################
### code chunk number 82: ASM_020.rnw:1725-1729
###################################################
#calcolo previsioni in corrispondenza di lambda =0 (OLS) newx matrice con osservazioni test data set
ridge.pred<-predict(ridge.mod,newx=x[-train,],s=0)
#stima MSE_test
mean((y.test-ridge.pred)^2)
summary(lm(y.test~x[-train,]))

###################################################
### code chunk number 84: ASM_020.rnw:1736-1745
###################################################
#confrontare e commentare valori MSE_test per lambda=c(4,1e10,0)
#per trovare il valore ottimo di lambda (con associato minimo MSE_test) possiamo effettuare cross validation (funzione cv.glmnet)
#di default 10-fold cv
set.seed(1234)
train<-sample(n,n/2)
cv.out<-cv.glmnet(x[train,],y[train],lambda=5*10^seq(-2,1,length=100),alpha=0)
#grafico con stima 10fold cv valori MSE_test in funzione di log(lambda)
plot(cv.out)


###################################################
### code chunk number 86: ASM_020.rnw:1757-1768
###################################################
#lambda con minimo valore stima 10fold 
(best.lambda<-cv.out$lambda.min)
#stima MSE_test in corrispondenza di best.lambda
ridge.pred<-predict(ridge.mod,newx=x[-train,],s=best.lambda)
mean((y.test-ridge.pred)^2)
#stima parametri su intero data set
ridge.mod<-glmnet(x,y,alpha=0)
predict(ridge.mod,s=best.lambda,type="coefficients")[1:12,]
#nessuno dei coefficienti e' stato posto pari a zero si osserva che Limit che risultava collineare con Rating ha coefficiente in valore assoluto piu' piccolo
#confronto con OLS
lm(y~x)


###################################################
### code chunk number 88: ASM_020.rnw:1775-1815
###################################################
#figura parametri in funzione di \lambda
#semistd coefs
layout(matrix(1:2,1,2))
x1<-scale(x)
grid<-c(10^seq(from=5,to=-2,length=1000))
ridge.mod<-glmnet(x1,y,alpha=0,lambda=grid)
##############ridge.mod<-glmnet(x1,y1,alpha=0,lambda=grid,standardize.response=TRUE)
coef.lambda<-sapply(1:length(grid), function(i) coef(ridge.mod)[,i])
coef.lambda<-t(coef.lambda[-1,])
plot(1,1,type="n",xlim=range(grid),ylim=c(-300,400),log="x",xlab=expression(lambda),ylab="Standardized Coefficients")
sapply(1:11, function(i) lines(grid,coef.lambda[,i]))
sapply(1:11, function(i) text(grid[1000-100],coef.lambda[1000-100,i],colnames(x1)[i],cex=2))
l2.norm<-function(x) sum(x^2)^.5
grid1<-apply(coef.lambda,1,l2.norm)/l2.norm(coef(lm(y~scale(x1))))
plot(0,0,type="n",xlim=range(grid1),ylim=c(-300,400),xlab="dimensione effetto shrinkage rispetto a OLS",ylab="Standardized Coefficients")
sapply(1:11, function(i) lines(grid1,coef.lambda[,i]))
sapply(1:11, function(i) text(grid1[1000-400],coef.lambda[1000-400,i],colnames(x1)[i],cex=2))


###################################################
### code chunk number 90: ASM_020.rnw:1828-1838
###################################################
#griglia valori per parametro tuning \lambda
grid<-c(10^seq(from=10,to=-2,length=100),0)
#Lasso
#funzione glmnet: argomento alpha=1 --> lasso
lasso.mod<-glmnet(x,y,alpha=1,lambda=grid)
#le variabili sono di default standardizzate (standardize=TRUE) 
dim(coef(lasso.mod))
#coefficienti per lambda in 90 posizione nel vettore grid
grid[90]
coef(lasso.mod)[,90]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(abs(coef(lasso.mod)[-1,90]))


###################################################
### code chunk number 91: ASM_020.rnw:1845-1854
###################################################
#coefficienti per lambda=0 in posizione 101 nel vettore grid
lasso.mod$lambda[101]
coef(lasso.mod)[,101]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(abs(coef(lasso.mod)[-1,101]))
#stima OLS
coef(lm(y~x))
#per ottenere valori precisi con glmnet occorre aumentare il parametro di precisione dell'algoritmo iterativo
coef(glmnet(x,y,alpha=1,lambda=grid,thresh=1e-19))[,101]


###################################################
### code chunk number 92: ASM_020.rnw:1861-1871
###################################################
#coefficienti per lambda=40 in posizione 1 nel vettore grid
lasso.mod$lambda[40]
coef(lasso.mod)[,40]
#corrispondente valore della norma in l_2 (tolta intercetta)
sum(abs(coef(lasso.mod)[-1,1]))
#stima OLS modello costante
coef(lm(y~1))
mean(y)
#per ottenere valori precisi con glmnet occorre aumentare il parametro di precisione dell'algoritmo iterativo
coef(glmnet(x,y,alpha=1,lambda=grid))[,1]


###################################################
### code chunk number 93: ASM_020.rnw:1878-1885
###################################################
#stima MSE_test
#definizione indici per training data set
set.seed(1400) # ATTENZIONE SEED MODIFICATO RISPETTO A VERSIONE CARICATA LUNEDI
train<-sample(n,n/2)
y.test<-y[-train]
#stima modello Lasso su training data set
lasso.mod<-glmnet(x[train,],y[train],alpha=1,lambda=grid)


###################################################
### code chunk number 95: ASM_020.rnw:1892-1897
###################################################
#per trovare il valore ottimo di lambda (con associato minimo MSE_test) possiamo effettuare cross validation (funzione cv.glmnet)
#di default 10-fold cv
cv.out<-cv.glmnet(x[train,],y[train],lambda=5*10^seq(-2,1,length=100),alpha=1)
#grafico con stima 10fold cv valori MSE_test in funzione di log(lambda)
plot(cv.out)


###################################################
### code chunk number 97: ASM_020.rnw:1909-1921
###################################################
#lambda con minimo valore stima 10fold 
(best.lambda<-cv.out$lambda.min)
#stima MSE_test in corrispondenza di best.lambda
lasso.pred<-predict(lasso.mod,newx=x[-train,],s=best.lambda)
mean((y.test-lasso.pred)^2)
#stima parametri su intero data set
lasso.mod<-glmnet(x,y,alpha=1)
predict(lasso.mod,s=best.lambda,type="coefficients")[1:12,]
#5 coefficienti sono stati posti pari a zero si osserva che Limit che risultava collineare con Rating ha coefficiente quasi nullo
#confronto con OLS
lm(y~x)
#ma riprovare con set.seed diverso


###################################################
### code chunk number 99: ASM_020.rnw:1928-1943
###################################################
#figura parametri in funzione di \lambda
layout(matrix(1:2,1,2))
x1<-scale(x)
grid<-c(seq(from=5000,to=1,length=10000))
ridge.mod<-glmnet(x1,y,alpha=1,lambda=grid)
coef.lambda<-sapply(1:length(grid), function(i) coef(ridge.mod)[,i])
coef.lambda<-t(coef.lambda[-1,])
plot(1,1,type="n",xlim=range(grid),ylim=c(-300,400),log="x",xlab=expression(lambda),ylab="Standardized Coefficients")
sapply(1:11, function(i) lines(grid,coef.lambda[,i]))
sapply(1:11, function(i) text(grid[10000-2],coef.lambda[10000-2,i],colnames(x1)[i],cex=2))
l1.norm<-function(x) sum(abs(x))
grid1<-apply(coef.lambda,1,l1.norm)/l1.norm(coef(lm(y~scale(x1)))[-1])
plot(0,0,type="n",xlim=range(grid1),ylim=c(-300,400),xlab="dimensione effetto shrinkage rispetto a OLS",ylab="Standardized Coefficients")
sapply(1:11, function(i) lines(grid1,coef.lambda[,i]))
sapply(1:11, function(i) text(grid1[10000-10],coef.lambda[10000-10,i],colnames(x1)[i],cex=2))


###################################################
### code chunk number 101: ASM_020.rnw:1956-2003
###################################################
library(glmnet)
library(mvtnorm)

decomposition<-function(f,sigmaE,alpha) {
  varcov<-diag(45)
  varcov[1,2]<-varcov[2,1]<-.99
  varcov[2,3]<-varcov[3,2]<-.99
  varcov[1,3]<-varcov[3,1]<-.99
  varcov[5,6]<-varcov[6,5]<-.57
  varcov[6,7]<-varcov[7,6]<- -.06
  varcov[5,7]<-varcov[7,5]<-.78
  varcov[15,16]<-varcov[16,15]<-.57
  varcov[16,17]<-varcov[17,16]<- -.06
  varcov[15,17]<-varcov[17,15]<-.78
  varcov[25,26]<-varcov[26,25]<-.57
  varcov[26,27]<-varcov[27,26]<- -.06
  varcov[25,27]<-varcov[27,25]<-.78
  varcov[35,36]<-varcov[36,35]<-.57
  varcov[36,37]<-varcov[37,36]<- -.06
  varcov[35,37]<-varcov[37,35]<-.78
  
  set.seed(1234)
  testxobs<-cbind(1,rmvnorm(ntest,sigma=varcov))
  testyobs<-f(testxobs[,-1])+rnorm(ntest,sd=sigmaE)
  decompositionrr<-function(f) {
    x<-cbind(1,rmvnorm(ntrain,sigma=varcov))
    y<-f(x[,-1])+rnorm(ntrain,sd=sigmaE)
    ridge.mod<-glmnet(x,y,alpha=alpha,lambda=grid)
    predict(ridge.mod,newx=testxobs,s=grid)
  }
  a<-replicate(1000, decompositionrr(f))
  decomp<-rbind(
    apply((f(testxobs[,-1])-apply(a,c(1,2),mean))^2,2,mean),
    apply(apply(a,c(1,2),var),2,mean)
  )
  plot(grid,decomp[1,],type="l",ylim=c(0,ceiling(max(decomp)+sigmaE^2)),xlab="",ylab="",bty="n",col="blue",log="x")
  lines(grid,decomp[2,],col="red")
  lines(grid,sigmaE^2+apply(decomp,2,sum),lwd=3)
  abline(h=sigmaE^2,lty=2)
}

p<-45 #dispari
ntrain<-50
ntest<-100
f1<-function(X) c(1+X%*%beta1)
f2<-function(X) c(1+X%*%beta2)
grid<-10^seq(from=4,to=-2,length=100)


###################################################
### code chunk number 103: ASM_020.rnw:2013-2018
###################################################
layout(matrix(1:2,1,2))
beta2<-beta1<-runif(p)*2.5
beta1[30:p]<-beta1[30:p]/2
decomposition(f1,sigmaE=5,alpha=0) #ridge
decomposition(f1,sigmaE=5,alpha=1) #lasso


###################################################
### code chunk number 107: ASM_020.rnw:2038-2043
###################################################
layout(matrix(1:2,1,2))
beta2<-beta1<-runif(p)*2.5
beta2[3:p]<-0
decomposition(f2,sigmaE=5,alpha=0) #ridge
decomposition(f2,sigmaE=5,alpha=1) #lasso


###################################################
### code chunk number 116: ASM_020.rnw:2205-2218
###################################################
#lettura dati
credit <- read.table("credit.csv",sep=",",header=T)
#la prima variabile contiene gli identificativi u.s.
credit<-credit[,-1]
#numero u.s.
n<-nrow(credit)
#check su not available
max(is.na(credit))
x<-model.matrix(Balance~.,data=credit)[,-1]
y<-credit$Balance
#estrazione componenti principali con funzioni prcomp oppure princomp
X_PC <- prcomp(x,scale.=TRUE,data=x)
X_PC <- princomp(x,cor=TRUE)


###################################################
### code chunk number 118: ASM_020.rnw:2228-2230
###################################################
#grafico screeplot
screeplot(X_PC,type="lines",npcs=11)


###################################################
### code chunk number 120: ASM_020.rnw:2243-2248
###################################################
#regressione con componenti principali
library(pls)
pcr.fit<-pcr(Balance~.,data=credit,scale=TRUE,validation="CV")
summary(pcr.fit)
# Root MSE is reported


###################################################
### code chunk number 122: ASM_020.rnw:2258-2260
###################################################
# Grafico con MSE
validationplot(pcr.fit,val.type="MSEP")


###################################################
### code chunk number 124: ASM_020.rnw:2273-2277
###################################################
#test MSE performance evaluation
set.seed(1234)
train<-sample(n,n/2)
pcr.fit<-pcr(Balance~.,data=credit,subset=train,scale=TRUE,validation="CV")


###################################################
### code chunk number 126: ASM_020.rnw:2287-2289
###################################################
# Grafico con MSE
validationplot(pcr.fit,val.type="MSEP")


###################################################
### code chunk number 128: ASM_020.rnw:2302-2310
###################################################
pcr.pred<-predict(pcr.fit,x[-train,],ncomp=11)
mean((pcr.pred-y[-train])^2)
#il modello scelto coincide con quello che avevamo inizialmente stiamato con 10 componenti (lo ristimiamo su tutto data set)
pcr.fit<-pcr(Balance~.,data=credit,scale=TRUE,validation="CV")
#e' possibile estrarre loadings e coefficienti
loadings(pcr.fit)
coef(pcr.fit,10)
coef.pcr<-coef(pcr.fit,1:11)[1:11,,1:11]


###################################################
### code chunk number 130: ASM_020.rnw:2320-2325
###################################################
#grafico andamento coefficienti per le soluzioni con diverso numero componenti principali
col<-rainbow(11)
plot(0,0,type="n",xlim=c(1,11),ylim=range(coef.pcr),xlab="n. componenti principali",ylab="coef. std.")
sapply(1:11,function(i) {par(new=TRUE);plot(1:11,coef.pcr[i,],type="S",xlim=c(1,11),ylim=range(coef.pcr),xlab="",ylab="",col=col[i])})
sapply(1:11,function(i) text(11,coef.pcr[i,11],rownames(coef.pcr)[i]))
