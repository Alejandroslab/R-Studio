#########################################################################
######## Importazione del dataset e salvataggio del file .rda ###########
#########################################################################

DATI <- read.table("dati.csv", header = TRUE, sep = ";")
data <- data.frame(DATI)
getwd()
save (data, file="data.rda")
load("data.rda") 

# assegnazione dei valori

data$gender <- factor(data$gender)
levels(data$gender) <- c("Maschio", "Femmina")

data$jobcat <- factor(data$jobcat)
levels(data$jobcat) <- c("Impiegato", "Intermedio", "Dirigente")
data$jobcat <- ordered(data$jobcat)

data$minority <- factor(data$minority)
levels(data$minority) <- c("Non Appartiene", "Appartiene")

save (data, file="data.rda")

attach(data)
# detach(data)

######################### distribuzioni di frequenza#####################

# qualitativo nominale

table(gender) # distribuzione delle frequenze assolute
table(gender)/length(gender) # distribuzione delle frequenze relative
table(gender)/length(gender)*100 # distribuzione delle frequenze percentuali

# qualitativo ordinale

table(jobcat) # distribuzione delle frequenze assolute
table(jobcat)/length(jobcat) # distribuzione delle frequenze relative
table(jobcat)/length(jobcat)*100 # distribuzione delle frequenze percentuali

cumsum(table(jobcat)) # distribuzione delle frequenze assolute cumulate
cumsum(table(jobcat)/length(jobcat)) # distribuzione delle frequenze relative cumulate
cumsum(table(jobcat)/length(jobcat)*100) # distribuzione delle frequenze percentuali cumulate

# quantitativo discreto

table(educ) # distribuzione delle frequenze assolute
table(educ)/length(educ) # distribuzione delle frequenze relative
table(educ)/length(educ)*100 # distribuzione delle frequenze percentuali

cumsum(table(educ)) # distribuzione delle frequenze assolute cumulate
cumsum(table(educ)/length(educ)) # distribuzione delle frequenze relative cumulate
cumsum(table(educ)/length(educ)*100) # distribuzione delle frequenze percentuali cumulate

# quantitativo continuo

str(salary)
str(salbegin)

min(salary)
max(salary)
min(salbegin)
max(salbegin)

table(cut(salary, breaks=c(15000,20000,30000,50000,135000)))
table(cut(salary, breaks=c(15000,20000,30000,50000,140000), right=FALSE))
hist(salary, breaks=c(15000,20000,30000,50000,135000), plot=T)

##################### Rappresentazioni grafiche ######################
######################################################################

# Grafico per settori circolari o torta

pie(table(gender))
pie(table(gender)/sum(table(gender)))
pie(table(minority))

plot(jobcat)
plot(table(educ))
plot(table(educ)/length(educ))

hist(salary, breaks=c(15000,20000,30000,50000,135000))
hist(salary, main="Sturges") # (x(n)-x(1))/(log_2(n+1))
hist(salary, breaks="FD", main="Freedman-Diaconis") # 2(Q3-Q1)n(-1/3)
hist(salary, breaks=11, main="11 classi") # diviso in 11 classi

hist(salary, breaks=11, plot=FALSE)

################################################
################# II parte #####################
################################################

###### Importazione del dataset e salvataggio del file .rda #######

DATI <- read.table("dati.csv", header = TRUE, sep = ";")
data <- data.frame(DATI)
save (data, file="data.rda")

load("data.rda") 

attach(data)
# detach(data)

###### Funzioni di base ######

# minimo, massimo e range

minimo <- min(salary)
minimo

massimo <- max(salary)
massimo

range <- range(salary)
range

campo_variazione <- max(salary)-min(salary)
campo_variazione

# mediana

mediana <- median(educ)
mediana

# quartili-quantili

quartile <- quantile(salary)
quartile

quantile_30 <- quantile(salary, probs=c(0.3))
quantile_30

# media

media <- mean(salary)
media

media_t1 <- mean(salary, trim=0.1)
media_t1

############ Funzione "summary" #############

summary(gender)


################ Dispersione ################

# varianza campionaria

var_c <- var(salary)
var_c

sd_c <- sd(salary)
sd_c

#############################################
########## esempio di function ##############
#############################################

# varianza

sigma2_1 <- function(x)
{
  var(x)*(length(x)-1)/length(x)
}

sigma2_1_salary <- sigma2_1(salary)
sigma2_1_salary

sigma2_2 <- function(x)
{
  mean((x-mean(x))^2)
}

sigma2_2_salary <- sigma2_2(salary)
sigma2_2_salary

# coefficiente di variazione

cv <- function(x)
{
  sqrt(sigma2_2(x))/abs(mean(x))
}

cv_salary <- cv(salary)
cv_salary

######### Rimozione oggetti #################

rm(list=ls(all=TRUE))

###### PLOT #######

# type: p, l, h
# main/sub/xlab/ylab

? cars

data(cars)
str(cars)
head(cars)

plot(cars$speed, cars$dist)

attach(cars)
plot(speed, dist)

plot(cars, type="l")
plot(cars, type="h")

plot(cars, main = "Speed and stopping distances for cars",
     sub = "Source: M. Ezekiel, Methods of Correlation Analysis. Wiley, 1930",
     xlab = "Speed (miles per hour)", ylab = "Stopping distance (feet)")

###### salvare i grafici

bmp(file = "cars.bmp")
plot(cars, main = "Speed and stopping distances for cars",
     sub = "Source: M. Ezekiel, Methods of Correlation Analysis. Wiley, 1930",
     xlab = "Speed (miles per hour)", ylab = "Stopping distance (feet)")
dev.off()

jpeg(file = "cars.jpg", quality = 20)
plot(cars, main = "Speed and stopping distances for cars",
     sub = "Source: M. Ezekiel, Methods of Correlation Analysis. Wiley, 1930",
     xlab = "Speed (miles per hour)", ylab = "Stopping distance (feet)")
dev.off()

postscript(file = "cars.ps")
plot(cars, main = "Speed and stopping distances for cars",
     sub = "Source: M. Ezekiel, Methods of Correlation Analysis. Wiley, 1930",
     xlab = "Speed (miles per hour)", ylab = "Stopping distance (feet)")
dev.off()

pdf(file = "cars.pdf", paper = "A4")
plot(cars, main = "Speed and stopping distances for cars",
     sub = "Source: M. Ezekiel, Methods of Correlation Analysis. Wiley, 1930",
     xlab = "Speed (miles per hour)", ylab = "Stopping distance (feet)")
dev.off()


###### Time series plots

# >>>>>>>>>> !!!!! tilde "ALT+126"  !!!!! <<<<<<<<

# importare i dati

url <- "http://people.su.se/~ma/R_intro/DataWageMacro.rda"
download.file(url, "DataWageMacro.rda")
load("DataWageMacro.rda")

# time index is x coordinates

plot(macro)

ts.plot(macro)
ts.plot(macro[, "cci"])
ts.plot(macro[, 2])

# par()

?par

# line decorations, legends and colours

# lty: 1=solid=continua, 2=dashed=tratteggiata,
# 3=dotted=punteggiata, 4=dotdash=punto trattino, 
# 5=longdash=trattino lungo, 6=twodash=due trattini

ts.plot(macro, gpars = list(lty = c(1,2,6)))

# spessore lwd=1 (default)

ts.plot(macro, gpars = list(lty = c(1,2,6), lwd=2))

# colore 

ts.plot(macro, gpars = list(lty = c(1,2,6),
                            lwd=2, col = c("black", "red", "blue")))

# legenda legend(d) (top/bottom - righ/left/center

legend("topright", c("CCI", "QMACRO", "QMICRO"),
       lty = c(1,2,6), col = c("black", "red", "blue"))

# colore dello sfondo

par(bg = "lightblue")
ts.plot(macro, gpars = list(lty = c(1,2,6),
                            lwd=2, col = c("black", "red", "blue")))
legend("bottomright", c("CCI", "QMACRO", "QMICRO"),
       lty = c(1,2,6), col = c("black", "red", "blue"))

par(mar = c(5,4,4,8))
ts.plot(macro, gpars = list(lty = c(1,2,6),
                            lwd=2, col = c("black", "red", "blue")))

par(bg = "white")
par(mar = c(5,4,4,8))
ts.plot(macro, gpars = list(lty = c(1,2,6),
                            lwd=2, col = c("black", "red", "blue")))

### carico un altro files di dati

?mtcars
data(mtcars)
str(mtcars)

attach(mtcars)

plot(mtcars)
plot(mpg, cyl)

plot(mpg, cyl, pch = 10)

plot(mpg, cyl, pch = 6, xlab = "Consumo",
     ylab = "Cilindrata", col.axis = "blue", col.lab = "red")

###############################
### creo nuovi dati
###############################

x <- c(1:10)
y <- x
z <- 10/x

plot(x,y, type = "b", pch = 2, col = "red", lty =5)
plot(x,z, type = "b", pch = 10, col = "blue", lty =3)
axis(4, at = z, labels = round(z, digits = 2), 
     col.axis = "red", las = 2, cex.axis = 0.5, tck = -.01)
mtext("y=10/x", side = 4, line = 3, cex.lab = 1, col = "blue")

axis(3, at = z, labels = round(z, digits = 2), 
     col.axis = "red", las = 2, cex.axis = 0.5, tck = -.01)

par(mar = c(5,4,4,5))

plot(x,z, type = "b", pch = 10, col = "blue", lty =3, yaxt = "n",
     xlab = "", ylab = "")
axis(4, at = z, labels = round(z, digits = 2), 
     col.axis = "red", las = 2, cex.axis = 0.5, tck = -.01)
mtext("y=10/x", side = 4, line = 3, col = "blue")

abline(h = mean(x), v = mean(y))

# 4 figure in 2 righe e 2 colonne

par(mfrow=c(2,2))
plot(wt, mpg, main = "wt vs mpg")
plot(wt, disp, main = "wt vs disp")
hist(wt, main = "histogram of wt")
boxplot(wt, main = "Boxplot of wt")

# 3 figure in 3 righe 1 colonna

par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)

# layout function

layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)

# aggiungere un boxplot ad uno scatter plot

par(fig=c(0,0.8,0,0.8), new=TRUE)
plot(wt, mpg, xlab=" Peso dell'automezzo", ylab="Consumo")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(wt, horizontal=T, axes=F)
par(fig=c(0.65,1,0,0.8), new=TRUE)
boxplot(mpg, vertical=T, axes=F)

# fig=c(x1,x2,y1,y2), new=TRUE aggiunge a una figura esistente

######### APPLY #########

# permette di calcolare una funzione sulle righe sulle colonne di una matrice
# La sintassi "apply(X, dim, FUN)"
# X è la matrice di dati
# dim è la dimensione 1 = riga, 2 = colonna
# FUN è la funzione da applicare

# genero la matrice m
m <- matrix(data=cbind(1:30,rep(1:5, 6), rep(1:5, each=6)),nrow=30, ncol=3)

# (3 metodi, 30 sottoinsiemi)

# calcolo le medie per colonna
m_c <- apply(m, 2, mean)
m_c

# calcolo le medie per riga
m_r <- apply(m, 1, mean)
m_r

# conto i numeri < 15
apply(m, 2, function(x) length(x[x<15]))

# medie tra i numeri > 3
apply(m, 2, function(x) mean(x[x>3]))

######### SAPPLY - LAPPLY #########

sapply(1:3, function(x) x^2)
lapply(1:3, function(x) x^2)

sapply(1:3, function(x) x^2, simplify=F)
unlist(lapply(1:3, function(x) x^2))

sapply(1:2, function(x) mean(m[,x]))
sapply(1:2, function(x, y) mean(y[,x]), y=m)


##############################################
################advanced commands ############
##############################################

### es. processo poisson composto con sapply####

#### reset settaggio grafico
op <- par(no.readonly = TRUE)
par(op)

#primaria Poisson
lambda <- 1000
# secondaria logN
media <- 8
devstd <- 0.05

# n.b.  E(X) = exp(?? + 1/2 ??^2)
#       Var(X) = exp(2*?? + ??^2)*(exp(??^2) - 1)
exp(media + 1/2*devstd^2) # E(X)  
exp(2*media + devstd^2)*(exp(devstd^2) - 1) # Var(X)

severity <- function(i, lambda1, media1, devstd1) sum(rlnorm(rpois(1, lambda=lambda1), meanlog = media1, sdlog = devstd1))

system.time(
Tot_lossess <- sapply(1:1000, severity, lambda1=lambda, media1=media, devstd1=devstd)
)



hist(Tot_lossess, freq=F)
curve(dnorm(x, mean = mean(Tot_lossess), sd = sd(Tot_lossess)), 
      from=min(Tot_lossess), to=max(Tot_lossess), add=T)

media <- mean(Tot_lossess)
abline(v=media, col="green", lwd=2)

media.con.exp.profit <- media*1.02
abline(v=media.con.exp.profit, col="red", lwd=2)

VaR <- quantile(Tot_lossess, probs=.995)
abline(v=VaR, col="blue", lwd=2)

TVaR <- mean(Tot_lossess[Tot_lossess>VaR])
abline(v=TVaR, col="brown", lwd=2)

Unexp.lossess.TVaR <- TVaR - media
Unexp.lossess.VaR <- VaR - media

Capital.Required <- VaR -media.con.exp.profit

Exp.profit <- media.con.exp.profit-media
Exp.lossess <- media
legend("topleft",c("media", "media con exp profit", "VaR", "TVaR"), lty=1,
       col=c("green", "red", "blue", "brown"), lwd=2, cex=.8)

################################################
################ III parte #####################
################################################

# random numbers

set.seed(123)
d1 <- rnorm(5)
d1
d2 <- rnorm(7)
d2

############### cicli for ##################

somma <- 0

for(i in 1:10)
{
  somma <- somma+i
  somma
}

somma

############## IF #############

x_1 <- 3
x_2 <- 5
if (x_2 > 2) y_1 <- 2*x_1
y_1

if (x_2<2) y_2 <- 2*x else y_2 <- 3*x_1
y_2

corplot <- function(x, y, p)
{
  if (p >2)
    plot(x, y)
  cor(x, y)
}
x_3 <- seq(1, 10)
x_3
y_3 <- seq(11,20)
y_3

corplot(x_3,y_3,4)

##### Distribuzione Normale ########

# Densità
n_1 <- dnorm(x_3,mean=0,sd=1)
n_1

# Funzione di ripartizione
n_2 <- pnorm(1.96,0,1)
n_2

# Quantile
n_3 <- qnorm(0.99, 0, 1)
n_3

# Generazione di numeri casuali
n_4 <- rnorm(5,0,1)
n_4

# grafico Normale standardizzata
curve(dnorm(x, mean = 0, sd = 1),-5, 5)

# qq-plot

x.norm <- rnorm(10, 2, 1)
x.norm
## standardizziamo i dati
z.norm <- (x.norm-mean(x.norm))/sd(x.norm)
z.norm
mean_z.norm <- mean(z.norm)
mean_z.norm
sd_z.norm <- sd(z.norm)
sd_z.norm

qq <- qqnorm(z.norm) ## tracciamo il QQ plot
abline(0,1) ## tracciamo la diagonale

qq <- qqnorm(x.norm) ## tracciamo il QQ plot
qq_l <- qqline(x.norm)

#### pulizia workspace
ls()
rm(list=ls(all=TRUE))
ls()


########################################
########### verosimiglianza ############

par(mfrow=c(1,1))

###############################################
# verosim bernoulli
from_bern <- rbinom(100, size = 1, prob = .3)
summary(from_bern)

loglik <- function(p, y ) sum(y*log(p)+(1-y)*log(1-p))
probs <- seq(0.001, .999, by=0.001)
verosim <- sapply(probs, loglik, y=from_bern )

#verosim 
plot(probs, verosim, pch=16, cex=.1)

#verosim normalizzata
plot(probs, exp(verosim-max(verosim)), type="l", 
     pch=16, cex=.1, main="verosimiglianza normalizzata")

# interv di max verosim
alpha <- 0.05 
estremi <- c(min(probs[exp(verosim-max(verosim)) >alpha]) , 
             max(probs[exp(verosim-max(verosim)) >alpha]))
liv_conf <- rep(alpha,2)
lines(estremi, liv_conf, type="h")


# se il campione raddopppia
from_bern <- rep(from_bern, times = 2)
verosim <- sapply(probs, loglik, y=from_bern )
lines(probs, exp(verosim-max(verosim)), pch=16, cex=.1, col="red")

legend("topright", legend = c("n", "2*n"), 
       col=c("black", "red"), lty=c(1,2))

# interv di max verosim
alpha <- 0.05 
estremi <- c(min(probs[exp(verosim-max(verosim)) >alpha]) , 
             max(probs[exp(verosim-max(verosim)) >alpha]))
liv_conf <- rep(alpha,2)
lines(estremi, liv_conf, type="h", col="red")


#######################
# stima MLE con MASS
library(MASS)
?fitdistr
fitting <- fitdistr(from_bern, dbinom, list(prob = 0.5),size=1,  lower = 0.0001, upper = .99999)
fitting
confint(fitting)

#######################
# stima MLE con stats4
library(stats4)
?mle
loglik <- function(prob) -sum(stats:: dbinom(x = from_bern, size = 1, prob, log = T))
mle(loglik, start=list(prob=.5),  method = "Brent", lower = 0, upper = 1)

confint(mle(loglik, start=list(prob=.5)))


###############################################
# verosim media gaussiana (sigma noto)
from_norm <- rnorm(100,mean = 4, sd = 2)
summary(from_norm)

loglik <- function(mean) sum(stats::dnorm(x = from_norm, mean, sd=2, log = T))
medie <- seq(2,6, by=0.01)
verosim <- sapply(medie, loglik )

#verosim 
plot(medie, verosim, pch=16, cex=.1)

#verosim normalizzata
plot(medie, exp(verosim-max(verosim)), 
     pch=16, cex=.1, type="l", main="verosim normalizzata")

# stima MLE con MASS
library(MASS)
stima <- fitdistr(from_norm, densfun = "normal", sd=1) ### !!!
stima$estimate
stima$vcov
confint(stima)

# stima MLE con stats4
library(stats4)
loglik <- function(mean) -sum(stats::dnorm(x = from_norm, mean, sd=2, log = T))
confint(mle(loglik, start=list(mean=3)))


###############################################
# distribuzione (simulata) stimatore pi.greco bernoulli
# stima MLE con stats4
library(stats4)

stimaML <- function(i,n, prob) {
  loglik <- function(prob1) -sum(stats::dbinom(x , size = 1, prob=prob1, log = T))
  x <- rbinom(n, size = 1, prob = prob)
  mle(loglik, start=list(prob1=.5), method = "Brent", lower = 0, upper = 1)@coef
}

system.time(
  vettore_pi <- sapply(1:1000, stimaML, n=100, prob=.3)
)

hist(vettore_pi, freq=F, 
     xlab="p", main="distribuzione di p Be(p)")
curve(dnorm(x, mean = mean(vettore_pi), sd = sd(vettore_pi)),
      from=0, to=1, add=T, col="red")
quantile(vettore_pi, probs=c(.025,.975))

#######################################################
# distribuzione asintotica del rapporto di verisimiglianza
# -2logL -> chi_quadro(gdl=1)
library(MASS)

loglik <- function(i,n,p_vero, p_H0) {
  y <- rbinom(n, size = 1, prob = p_vero)
  loglik_H0 <- sum(y*log(p_H0)+(1-y)*log(1-p_H0))
  fitting <- fitdistr(y, dbinom, list(prob = 0.5),size=1,  lower = 0.0001, upper = .99999)
  c(loglik_H0, unlist(fitting))
}

#test H0=p_vero : si ha la distribuzione chisq(1)
#test H0<>p_vero : si ha la distribuzione  chisq(1) noncentrale
repl <- 10000
n=500
p_vero=.5
p_H0=.4
verosim <- sapply(1:repl, loglik , n=n, p_vero=p_vero, p_H0=p_H0, simplify = T)
rownames(verosim)[1] <- "loglik_H0"
verosim <- t(as.matrix(verosim))

#v.c. di wilks
distrib_rapp_verosim <- -2*(verosim[,1]-verosim[,5])

hist(distrib_rapp_verosim, freq=F, nclass=50)
curve(dchisq(x,df = 1), from=0.1, to=25, add=T, col="red")

# soglia critica al 95%
lim_sup <- qchisq(p = .95,df = 1)
abline(v=lim_sup, col="green")
#p-value
pvalue <- sum(distrib_rapp_verosim<=lim_sup)/repl
paste("p-value: ", pvalue)
ifelse(pvalue >.05,("accetto H0"),  ("rifiuto H0"))

############################
# distribuzione (simulata) stimatore lambda Poisson
stimaML <- function(i,n, lambda) {
    loglik <- function(lambda1) -sum(stats::dpois(x ,lambda =  lambda1, log = T))
  x <- rpois(n, lambda = lambda)
  mle(loglik, start=list(lambda1=1), method = "Brent", lower = 0, upper=1e+10)@coef
}
vettore_lambda <- sapply(1:1000, stimaML, n=100, lambda=2)

hist(vettore_lambda , freq=F, 
     xlab=expression(lambda), 
     main=expression(paste("distribuzione di ", lambda, " Pois(",lambda,")")))
curve(dnorm(x, mean = mean(vettore_lambda), sd = sd(vettore_lambda)),
      from=min(vettore_lambda), to=max(vettore_lambda), add=T, col="red")

quantile(vettore_lambda , probs=c(.025,.975))


##############################################
# verosim  lognormal
##############################################

from_lnorm <- rlnorm(100,meanlog = 1, sdlog = 1)
summary(from_lnorm)

loglik <- function(param) sum(stats::dlnorm(x = from_lnorm, 
                              meanlog = param[1],sdlog = param[2], log = T))
medie <- seq(0,2, length.out = 201)
devstds <- seq(0.5,2, length.out = 201)

param_space <- cbind(rep(medie, 201), rep(devstds, each=201))

verosim <- apply(param_space,MARGIN = 1,  loglik )


#verosim 
sup <- matrix(verosim, 201,201, byrow=T)
persp(medie, devstds,sup, zlim=c(min(sup), max(sup)),
      theta = 30, phi = 30, expand = 0.5,
      col = "lightblue",ticktype="detailed",
      xlab="media",ylab="devstd",zlab="log_lik", main="verosimiglianza")

image(medie, devstds,sup, 
      col=rainbow(50),xlab="media", ylab="devstd",main="contour verosimiglianza")
contour(medie, devstds,sup, nlevels=75, add=T)




#################################
######### Regressione ###########
######### esempio     ###########
#################################

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

# normalità degli errori
qqnorm(scale(residui_1))
abline(0,1)


# diagnistica
coef_test_1 <- coeftest(lm_1)
coef_test_1

par(mfrow=c(2,2))
plot(lm_1)

lm_2 <- lm(salary~salbegin+educ+prevexp, data)
lm_2

summary_lm_2 <- summary(lm_2)
summary_lm_2

formula_2 <- formula(lm_2)
formula_2

testbp_2 <- bptest(formula_2, data=data)
testbp_2
testdw_2 <- dwtest(formula_2, data=data)
testdw_2

## Confronto tra Modelli

anova_12 <- anova(lm_1,lm_2)
anova_12


## ......


######################################
############ esempio simulato ########
# come creare una coppia di variabili assegnato un modello
# y= a+ b*x +eps      eps~N(0,sigma^2)
a <- 0
b <- 1
sigma <- 0.1

#genero il vettore x
x <- seq(1,10,.1)/10

#genero un vettore di num casuali da una normale
eps <- rnorm(length(x), mean=0, sd=sigma)

# genero y
y <- a+ b*x+ eps

# grafico di x e y
plot(x,y)

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
model.matrix(es_regr)
model.matrix(~x)


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

