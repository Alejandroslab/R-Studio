########################################################
########### lezioni introduttive a R ###################
####### Analisi Statistica Multivariata 2015-2016 ######
########## regressione lineare multipla ################
########################################################


####### Regressione  esempio negli appunti ###########
# se dati in memoria con ctrl+c : read.table("clipboard")

xx <- c(0.721,	2	, 42,
        0.933,	5 ,	37,
        0.863,	3 ,	43,
        1.333,	6 ,	33,
        1.784,	8 ,	38,
        2.534,	12,	28,
        3.419,	15,	23,
        1.961,	9 ,	41)

xx <- matrix(xx, ncol=3, byrow=T)
colnames(xx) <- c("y", "x1", "x2")
xx <- data.frame(xx)

# tilde "ALT+126"
X <- model.matrix(~x1+x2, data = xx) # matrice X "aumentata" con un vettore di 1 
X

crossprod(X,X) # X'X
solve(crossprod(X,X)) # inv(X'X)

P <- X %*% solve(crossprod(X,X)) %*% t(X) # X[inv(X'X)]X' hat matrix : matrice proiezione X(X'X)^-1 X'

Ident <- diag(rep(1,dim(P)[1])) # matrice identità

res <- (Ident - P) %*% xx$y # residui

crossprod(res,res) #  norma dei residui: devianza residua
crossprod(res,res)/(dim.data.frame(xx)[1]-2-1) # varianza  residua



###########################################################
## lm(): comando R per eseguire stima e inferenza su un modello di regressione
###########################################################
lm(y~x1+x2, data = xx)
summary(lm(y~x1+x2, data = xx))
###########################################################


####################################################################
### modello lineare gaussiano: esempio di simulazione di modello ###
### y= a+ b*x +eps      eps~N(0,sigma^2)
########################################################
# assegnazione parametri di popolazione
a <- 0
b <- 1
sigma <- 0.1

#creazione del vettore x (variab dipend)
x <- seq(1,10,.5)/10

#generazione  vettore di num casuali da una normale da assegnare alla componente errore
eps <- rnorm(length(x), mean=0, sd=sigma)

# genero y in base al modello
y <- a+ b*x+ eps

# scatterplot di x e y
plot(x,y)
# modello teorico
lines(x,a+b*x, col="red", lty=2)

# regressione di x su y
es_regr <- lm(y~x)
# per visualizzare le statistiche standard di un oggetto lm 
summary(es_regr)

# per elencare gli elementi di un oggetto
str(es_regr) 

# per estrarre gli elementi da un oggetto
es_regr$coefficients
es_regr$model
es_regr$residuals
model.matrix(es_regr) # equivalente a model.matrix(~x)

# valori interpolati
fitted <- predict(object = es_regr)
lines(x,fitted, type="l", col="blue")
legend("bottomright", legend = c("vero modello", "modello stimato"), 
      lty=c(2,1), col=c("red", "blue"))


####################################################################################################
# modello lineare classico gaussiano
# stima dei parametri con funzione di ottimo esplicita
####################################################################################################

mq <- function(param){
  sum((y-param[1]-param[2]*x)^2)
}
optim(par = c(0,0), fn = mq )

####################################################################################################
# modello lineare classico gaussiano
# stima dei parametri con massima verosimiglianza: verosim esplicita
####################################################################################################

### questa è - LogLik !!!
loglik <- function(param) -sum(dnorm(y-(param[1]+param[2]*x), sd=param[3], log = TRUE))

esempio <- optim(par = c(0,1,.5), fn = loglik,
                 method = "L-BFGS-B", 
                 lower = c(-Inf,-Inf, 0.00001), upper = c(+Inf, +Inf, +Inf))
esempio$par

### oppure
library(stats4)
loglik <- function(alpha=0, beta=0, devstd=1) -sum(dnorm(y-(alpha+beta*x), sd=devstd, log = TRUE))
esempio <- mle(loglik,start=list(alpha=0,beta=0,devstd=.5), 
               method = "L-BFGS-B", lower = c(-Inf,-Inf, 0.00001), upper = c(+Inf, +Inf, +Inf))
coef(esempio)


#############################################################
# distribuzione (simulata) stimatori dei parametri del modello y=a+b*x
# per verifica empirica delle proprietà teoriche degli stimatori
#############################################################

modello <- function(i, a,b, sigma){
  a1 <- a
  b1 <- b
  x <- seq(1,10,.1)/10
  eps <- rnorm(length(x), mean=0, sd=sigma)
  y <- a1+ b1*x+ eps
  regr <- lm(y~x)
  c(coef(regr), deviance(regr)^2/regr$df.residual, cor(x,y))
}

param <- sapply(1:1000, modello, a=0, b=1, sigma=.1)
param
param <- t(param)
param
param <- as.data.frame(param)
names(param) <- c("alpha", "beta", "var res", "rho2")
head(param)

# grafici
pairs(param)
hist(param[,1], main=names(param[1]))   # istogramma
boxplot(param[,1], main=names(param[1])) # boxplot

par(mfrow=c(2,4))
istogramma <- function(i,q){
  hist(q[,i], main=names(q[i]))   # istogramma
  }
sapply(1:4, istogramma, q=param)

boxplots <- function(i,q){
  boxplot(q[,i], main=names(q[i])) # boxplot
}
sapply(1:4, boxplots, q=param)

#############################################################
# esempio di modello gaussiano multivariato (simulato)
# Y = b0 + b1*X1+ b2*X2 + E    con E~N(0,sigma^2)
#############################################################

# inizializzo il generatore di numeri casuali
set.seed(123)

# assegnazione parametri beta
vec_beta <- c(1,2,3)

# assegnazione parametro sigma
sigma <- 1

# dimensione campionaria
n <- 100

# carico una libreria utile per generare poi numeri casuali da una gaussiana multivariata
library(MASS)

# assegnazione matr corr 
Vmat <- matrix(c(.3,.2,.2,.3),2,2)
Vmat

# generazione di due vettori da una v.c. gaussiana bivariata 
# vedi help(mvrnorm) (o ?mvrnorm)
X_mat <- mvrnorm(n=n,mu=c(1,1),Sigma=Vmat) 
plot(X_mat)

# creo un data frame
YX_mat <- data.frame(X_mat)

# assegno dei nomi ai campi del data frame
names(YX_mat) <- c("X1","X2")
head(YX_mat)

# esploro la struttura del modello di regressione (matrice X)
mod_matr <- model.matrix(~YX_mat$X1+YX_mat$X2)
head(mod_matr)

# genero il vettore Y=X*Beta+eps
Y <- mod_matr %*% as.vector(vec_beta) + rnorm(n, 0, sigma)
hist(Y,freq=FALSE)
curve(dnorm(x,mean(Y),sd(Y)),(mean(Y)-5*sd(Y)), (mean(Y)+5*sd(Y)), add=T)

# unisco Y a YX_mat
YX_mat <- data.frame(YX_mat, Y=Y)
Y_X1_X2 <- lm( Y ~ X1+X2, data=YX_mat)

# esploro il contenuto della regressione
names(Y_X1_X2)
str(Y_X1_X2)
summary(Y_X1_X2)
anova(Y_X1_X2)
predict(Y_X1_X2)
residuals(Y_X1_X2)
fitted(Y_X1_X2)
names(anova(Y_X1_X2))
anova(Y_X1_X2)$Df[3]

# considero un modello ridotto
Y_X1 <- lm( Y ~ X1, data=YX_mat)

#confronto i due modelli : valuto il miglioramento di varianza residua (o il duale varianza spiegata)
anova(Y_X1, Y_X1_X2)

# diagnostiche/grafici
par(mfrow=c(2,2))
plot(Y_X1_X2)

# calcolo varianza residua corretta coi gdl
var_res <- sum(Y_X1_X2$residuals^2)/anova(Y_X1_X2)$Df[3]

# calcolo parametri con algebra lineare: (X'X)^-1 X' y
solve(t(mod_matr) %*% (mod_matr)) %*% t(mod_matr) %*% Y 

# matr var-cov param
st_dev_param <- diag(var_res * solve(t(mod_matr) %*% (mod_matr)) )^.5
param_std <- Y_X1_X2$coefficients/st_dev_param 

# calcolo del p-value dei parametri
2*(1-pt(param_std, anova(Y_X1_X2)$Df[3], lower.tail = TRUE))

# calcolo F-stat value del modello
F_stat <- (sum((predict(Y_X1_X2)-mean(predict(Y_X1_X2)))^2)/(dim(mod_matr)[2]-1))/var_res

# p-value dell'F-stat  del modello
1-pf(F_stat,(dim(mod_matr)[2]-1), anova(Y_X1_X2)$Df[3], lower.tail = TRUE)


# interpolazione o previsione ?
par(mfrow=c(1,2))
library(mplot)

# nuovo dataset
X_mat_new <- as.data.frame(mvrnorm(n=10,mu=c(1,1),Sigma=Vmat) )
names(X_mat_new) <- c("X1","X2")

# interpolazione
pred_conf <- predict.lm(object = Y_X1_X2,   newdata =X_mat_new, interval="confidence", level=.999)
pred_conf <- pred_conf[order(pred_conf[,1]),]
matplot(pred_conf, type="l", ylim=c(0,20))

# previsione
pred_int <- predict.lm(object = Y_X1_X2, newdata =X_mat_new, interval="prediction", level=.999 )
pred_int <- pred_int[order(pred_int[,1])]
matplot(pred_int, type="l", ylim=c(0,20))

# valori influenti /hat matrix
library(stats)
diagres <- influence(Y_X1_X2) # elementi diagonali della hat matrix
names(diagres)
head(diagres$hat)
plot(diagres$hat)
1/n # soglia critica

#############################################################
# esempio di modello gaussiano multivariato (simulato)
# Y = b0 + b1*X1+ b2*X2 + b3*X3+ E    con E~N(0,sigma^2)
# verifica dell'effetto prodotto dalla presenza di un valore anomalo
# prodotto facendo cvariuare il livello medio
#############################################################

# inizializzo il generatore di numeri casuali
set.seed(123)

# assegnazione parametri beta
vec_beta <- c(1,1,1,1)

# assegnazione parametro sigma
sigma <- 1

# dimensione campionaria
n <- 99

# carico una libreria utile per generare poi numeri casuali da una gaussiana multivariata
library(MASS)
Vmat <- matrix(rep(.5,9),3,3)
diag(Vmat) <- 1
X_mat <- mvrnorm(n=n,mu=c(0,0,0),Sigma=Vmat) 

#inizializzo la matrice dei dati
YX_mat <- data.frame(X_mat)

coeff_out <- NULL
for(new_mean in seq(0,10, length.out = 25)){
  
  # repliche
  for(j in 1:100){

    # nuova osservazione con media modificata per la sola variabile X3
    X_mat_new <- mvrnorm(n=1,mu=c(0,0,new_mean),Sigma=Vmat) 
    # aggiungo il nuovo record
    YX_mat[100,] <- X_mat_new
    # esploro la struttura del modello di regressione (matrice X)
    mod_matr <- model.matrix(~., data = YX_mat)
    
    # genero il vettore Y=X*Beta+eps
    Y <- mod_matr %*% as.vector(vec_beta) + rnorm(n+1, 0, sigma)
    
    # unisco Y a YX_mat
    YX_mat_simu <- data.frame(YX_mat, Y=Y)
    Y_X1_X2_X3 <- lm( Y ~ ., data=YX_mat_simu)
    # memorizzo statistiche
    coeff_out <- rbind(coeff_out, c(new_mean,Y_X1_X2_X3$coeff,
                        influence(Y_X1_X2_X3)$hat[100], influence(Y_X1_X2_X3)$coefficients[100,]))
  }
}
coeff_out <- data.frame(coeff_out)
names(coeff_out) <- c("nuova_media", "beta0", "beta1", "beta2", "beta3",
                      "hat matr_new rec",  "variaz_beta0 tolto new rec", "variaz_beta1 tolto new rec", 
                      "variaz_beta2 tolto new rec", "variaz_beta3 tolto new rec")

with(coeff_out, aggregate(coeff_out, by=list(nuova_media), FUN=mean))

par(mfrow=c(3,3))
with(coeff_out, boxplot(beta0~as.factor(nuova_media), main="beta0", xlab="new_mean x3"))
with(coeff_out, boxplot(beta1~as.factor(nuova_media), main="beta1", xlab="new_mean x3"))
with(coeff_out, boxplot(beta2~as.factor(nuova_media), main="beta2", xlab="new_mean x3"))
with(coeff_out, boxplot(beta3~as.factor(nuova_media), main="beta3", xlab="new_mean x3"))
with(coeff_out, boxplot(coeff_out[,6]~as.factor(nuova_media), main="hat matr_new rec", xlab="new_mean x3"))
with(coeff_out, boxplot(coeff_out[,7]~as.factor(nuova_media), main="variaz_beta0 tolto new rec", xlab="new_mean x3"))
with(coeff_out, boxplot(coeff_out[,8]~as.factor(nuova_media), main="variaz_beta1 tolto new rec", xlab="new_mean x3"))
with(coeff_out, boxplot(coeff_out[,9]~as.factor(nuova_media), main="variaz_beta2 tolto new rec", xlab="new_mean x3"))
with(coeff_out, boxplot(coeff_out[,10]~as.factor(nuova_media), main="variaz_beta3 tolto new rec", xlab="new_mean x3)"))


#############################################################
# esempio di modello gaussiano multivariato (simulato)
# Y = b0 + b1*X1+ b2*X2 + b3*X3+ E    con E~N(0,sigma^2)
# verifica della stabilità NUMERICA dei parametri al crescere 
# della correlazione tra 2 regressori (COLLINEARITà)
#############################################################

# inizializzo il generatore di numeri casuali
set.seed(123)

# assegnazione parametri beta
vec_beta <- c(1,1,1,1)

# assegnazione parametro sigma
sigma <- 1

# dimensione campionaria
n <- 100

# carico una libreria utile per generare poi numeri casuali da una gaussiana multivariata
library(MASS)

Vmat <- matrix(rep(.5,9),3,3)
diag(Vmat) <- 1

coeff_out <- NULL
for(rho in seq(0.98,.9999, length.out = 25)){
  # assegnazione matr corr 
  Vmat[2,3] <- rho; Vmat[3,2] <- rho
  # repliche
  for(j in 1:100){
    # generazione di due vettori da una v.c. gaussiana bivariata 
    # vedi help(mvrnorm) (o ?mvrnorm)
    X_mat <- mvrnorm(n=n,mu=c(0,0,0),Sigma=Vmat) 
    # creo un data frame
    YX_mat <- data.frame(X_mat)
  
    # esploro la struttura del modello di regressione (matrice X)
    mod_matr <- model.matrix(~., data = YX_mat)
    
    # genero il vettore Y=X*Beta+eps
    Y <- mod_matr %*% as.vector(vec_beta) + rnorm(n, 0, sigma)
    # unisco Y a YX_mat
    YX_mat <- data.frame(YX_mat, Y=Y)
    Y_X1_X2_X3 <- lm( Y ~ ., data=YX_mat)
    coeff_out <- rbind(coeff_out, c(rho,Y_X1_X2_X3$coeff))
  }
}
coeff_out <- data.frame(coeff_out)
names(coeff_out) <- c("rho", "beta0", "beta1", "beta2", "beta3")

with(coeff_out, aggregate(coeff_out, by=list(rho), FUN=mean))
par(mfrow=c(2,2))
with(coeff_out, boxplot(beta0~as.factor(rho), main="beta0", xlab="rho(x2,x3)"))
with(coeff_out, boxplot(beta1~as.factor(rho), main="beta1", xlab="rho(x2,x3)"))
with(coeff_out, boxplot(beta2~as.factor(rho), main="beta2", xlab="rho(x2,x3)"))
with(coeff_out, boxplot(beta3~as.factor(rho), main="beta3", xlab="rho(x2,x3)"))



#############################################################
# esempio (esogeno) ulteriore
#############################################################

load("data.rda") 
attach(data)

cor(salary, salbegin)
plot(salary, salbegin)

# tilde "ALT+126"

lm_1 <- lm(salary~salbegin, data)
lm_1

coefficienti_1 <- coef(lm_1)
coefficienti_1

formula_1 <- formula(lm_1)
formula_1

devianza_residui_1 <-deviance(lm_1)
devianza_residui_1

residui_1 <- residuals(lm_1)
residui_1

vc_lm_1 <- vcov(lm_1)
vc_lm_1

summary_lm_1 <- summary(lm_1)
summary_lm_1

# media degli errori nulla
t_residui <- t.test(residui_1)
t_residui

################################
lm_2 <- lm(salary~salbegin+educ+prevexp, data)
lm_2

summary_lm_2 <- summary(lm_2)
summary_lm_2

formula_2 <- formula(lm_2)
formula_2

## Confronto tra Modelli

anova_12 <- anova(lm_1,lm_2)
anova_12
############################################


#################################################################################################
######## istruzioni avanzate: superficie di verosimiglianza al variare dello spazio dei parametri
#################################################################################################

par(mfrow=c(1,2))
alpha<- rep(seq(-.1,.1,length=100), each=100)
beta<- rep(seq(.8,1.13,length=100), times=100)

devstds <- coef(esempio)[3]*91/90
alpha_beta <- cbind(alpha, beta)

loglik <-function(param, devstd) sum(dnorm(y-(param[1]+param[2]*x), mean=0, sd=devstds, log = TRUE))

logliks <- NULL
for( i in 1:10000){
  logliks <- rbind(logliks, loglik(param=c(alpha_beta[i,]), devstd=devstds))
}

#### utilizzo del comando apply
#### logliks <- apply(alpha_beta, MARGIN=1, loglik, devstd=devstds)

sup <- matrix(logliks, 100,100, byrow=T)
persp(unique(alpha), unique(beta),sup,
      theta = 30, phi = 30, expand = 0.5,
      col = "lightblue",ticktype="detailed",
      xlab="alpha",ylab="beta",zlab="norm_lik", main="verosimiglianza")

image(unique(alpha), unique(beta),sup,, 
      col=rainbow(50),xlab="alpha", ylab="beta",main="contour verosimiglianza")
contour(unique(alpha), unique(beta),sup,
        nlevels=75,xlab="alpha", ylab="beta", add=T)


