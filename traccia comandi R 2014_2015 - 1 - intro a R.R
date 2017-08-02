####################################################################################################
# modello lineare classico gaussiano
# stima dei parametri con massima verosimiglianza: verosim esplicita
####################################################################################################
library(stats4)
loglik <- function(alpha=0, beta=0, devstd=1) -sum(dnorm(y-(alpha+beta*x), sd=devstd, log = TRUE))
esempio <- mle(loglik,start=list(alpha=0,beta=0,devstd=.5), 
               method = "L-BFGS-B", lower = c(-Inf,-Inf, 0.00001), upper = c(+Inf, +Inf, +Inf))
coef(esempio)


#################################################################################################
######## istruzioni avanzate: superficie di verosimiglianza al variare dello spazio dei parametri
#################################################################################################

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


# distribuzione (simulata) stimatori dei parametri del modello y=a+b*x
modello <- function(i, a,b, sigma){
  a1 <- a
  b1 <- b
  x <- seq(1,10,.1)/10
  eps <- rnorm(length(x), mean=0, sd=sigma)
  y <- a1+ b1*x+ eps
  regr <- lm(y~x)
  c(coef(regr), deviance(regr))
}
param <- as.data.frame(t(sapply(1:1000, modello, a=0, b=1, sigma=.1)))
names(param) <- c("alpha", "beta", "dev res")

pairs(param)

######## superficie di perdita con regressione robusta (median sum of abs residuals) TO BE COMPLETED
a1 <- 0
b1 <- 1
x <- seq(1,10,.1)/10
eps <- rnorm(length(x), mean=0, sd=sigma)
y <- a1+ b1*x+ eps

subset_anom <- floor(length(x)*.95):length(x) # 5% di valori anomali !!!
y[subset_anom] <-  a1+ b1*x[subset_anom]+ abs(eps[subset_anom])*10 


alpha<- rep(seq(-.1,.1,length=100), each=100)
beta<- rep(seq(.8,1.13,length=100), times=100)
parametri <- cbind(alpha, beta)


funz_obj <- function(i, param) sum(abs(y-(param[i,1]+param[i,2]*x)))
logliks <- sapply(1:10000, FUN = funz_obj, param=parametri)

sup <- matrix(logliks, 100,100, byrow=T)
library(rgl)
persp3d(unique(alpha), unique(beta),sup,zlim=c(10,13),
      theta = 30, phi = 30, expand = 0.5,
      col = "lightblue",ticktype="detailed",
      xlab="alpha",ylab="beta",zlab="norm_lik", main="verosimiglianza")
image(unique(alpha), unique(beta),sup, 
      col=rainbow(50),xlab="alpha", ylab="beta",main="contour verosimiglianza")
contour(unique(alpha), unique(beta),sup,
        nlevels=25,xlab="alpha", ylab="beta", add=T)

plot(y~x)
abline(lm(y~x))
plot(lm(y~x))
lm(y~x)


##### variabili dummy TO BE COMPLETED


#############################################
###### es di modello di regressione NON lineare
#############################################

### dataset tratto da dispensa

dati <- matrix(c(8 ,0.49,
10,	0.475,
12,	0.45,
14,	0.437,
16,	0.433,
18,	0.455,
20,	0.423,
22,	0.407,
24,	0.407,
26,	0.407,
28,	0.405,
30,	0.393,
32,	0.405,
34,	0.4,
36,	0.395,
38,	0.4,
40,	0.39,
42,	0.39), nrow=18, ncol=2, byrow=T)

dati <- data.frame(dati)
names(dati) <- c("x","y")

### Gauss Newton
Igrad <- function(param) 1-exp(-param*(dati$x-8))
IIgrad <- function(param) -(0.49-param[1])*(dati$x-8)*exp(-param[2]*(dati$x-8))
modello <- function(param) param[1]+(0.49-param[1])*exp(-param[2]*(dati$x-8))

### inizializzazione dei parametri
teta <- c(.1,.1)

### matrice dei gradienti numerici
Z <- cbind(Igrad(param=teta[2]),IIgrad(param=teta))

### ciclo Gauss-Newton per convergenza 
for(i in 1:10){
  teta <- as.vector(teta) + solve(t(Z) %*% Z) %*% t(Z) %*% (dati$y-modello(param=teta))
  Z <- cbind(Igrad(param=teta[2]),IIgrad(param=teta))
  print(c(t(teta), sum((dati$y-modello(teta))^2)))
  
}

# stima con OPTIM
per_otim <- function(param, data) {
  sum((data$y-(param[1]+(0.49-param[1])*exp(-param[2]*(data$x-8))))^2)
}  
optim(par = c(.1,.1), fn = per_otim, data=dati)$par


######################################## 
# distribuzione simulata dello stimatore dei parametri
########################################

sigma <- (sum(1/16*(dati$y-modello(teta))^2))^.5
#inizializzo un oggetto dove memorizzo le stime di b
teta_estimates <- NULL

for(i in 1:1000){
  #genero un vettore di num casuali da una normale
  eps <- rnorm(18, mean=0, sd=sigma)
  # genero y
  dati$y <- modello(teta)+ eps
  
  ### inizializzazione dei parametri
  teta_pert <- c(0.3,0.1)
  
  ### matrice dei gradienti numerici
  Z <- cbind(Igrad(param=teta_pert[2]),IIgrad(param=teta_pert))
  
  ### ciclo Gauss-Newton per convergenza 
  for(i in 1:10){
    teta_pert <- as.vector(teta_pert) + solve(t(Z) %*% Z) %*% t(Z) %*% (dati$y-modello(param=teta_pert))
    Z <- cbind(Igrad(param=teta_pert[2]),IIgrad(param=teta_pert))
  }
  
  # aggiungo (stack) la nuova stima alla base di un vettore
  teta_estimates <- rbind(teta_estimates, t(teta_pert))
}

par(mfrow=c(1,2))
apply(teta_estimates, MARGIN=2, hist, freq=F)
apply(teta_estimates, MARGIN=2, boxplot)
pairs(teta_estimates, labels = c("alpha", "beta"))


################################################
######## istruzioni avanzate
######## superficie di perdita
alpha<- rep(seq(.34,.43,length=20), each=20)
beta<- rep(seq(.035,.15,length=20), times=20)
alpha_beta <- cbind(alpha, beta)

funz_perdita <-function(param ) sum((dati$y-modello(param))^2)

perdita <- NULL
for( i in 1:20^2){
  perdita <- rbind(perdita, funz_perdita(param=alpha_beta[i,]))
}

#### utilizzo del comando apply
#### logliks <- apply(alpha_beta, MARGIN=1, loglik, devstd=devstds)

sup <- matrix(perdita, 20,20,byrow=T)
persp(unique(alpha), unique(beta),sup,theta = 120, phi = 25, expand = 1, col = rainbow(1),ticktype="detailed",xlab="alpha",ylab="beta",zlab="norm_lik", main="perdita")

image(unique(alpha), unique(beta),sup, 
      col=rainbow(50),xlab="alpha", ylab="beta",main="contour perdita")
contour(unique(alpha), unique(beta),sup,
        nlevels=35,xlab="alpha", ylab="beta", add=T)

### esercizio

