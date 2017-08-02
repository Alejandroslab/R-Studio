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

