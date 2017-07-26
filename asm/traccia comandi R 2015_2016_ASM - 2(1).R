########################################################
########### lezioni introduttive a R ###################
####### Analisi Statistica Multivariata 2015-2016 ######
########## Esempi di algebra lineare ##################
########################################################

# vettori
x <- c(1:3)/3 # vettore colonna
str(x)
is.vector(x)
is.matrix(x)
length(x)
t(x) # trasposta : vettore riga

y <- c(4:6)/6

#operazioni
x * y
x + y
x - y
x / y
x / (x*3)

#angolo fra due vettori
cor(x,y)

#se invece
y <- c(1,2,5)/5
cor(x,y)


#prodotto scalare
x %*% y 
t(x) %*% y
crossprod(x,y)
x %*% t(y)
tcrossprod(x,y)

#lunghezza
(x %*% x)^.5

x <- 3 #Scalar
c(1:3) #column vector	 	 	 
t(c(1:3)) #row vector	 	 	 
rep(1,times=2)  #vector of ones	 	 	 
rep(0,times=2) #vector of zeros	 	 	 


### rotazione "rigida" di un vettore
y <- c(0,1)
plot(0,0, xlim=c(-2,2), ylim=c(-2,2), col="red")
abline(h=0)
abline(v=0)

for(thet in seq(0,2*pi, length.out = 10)) {
  thet=as.numeric(format(thet, digits=3 ))
  costheta=cos(thet); sintheta=sin(thet)
  newcoord_x <- costheta
  newcoord_y <- sintheta
  lines(newcoord_x,newcoord_y, xlim=c(-2,2), ylim=c(-2,2), col="red")
  text(newcoord_x,newcoord_y, labels = (expression(theta)))
  text(newcoord_x,newcoord_y, labels = paste(" =", thet), adj=0)
  arrows(x0 = 0,y0 = 0,x1 = newcoord_x,y1 =newcoord_y)
}

curve(sqrt(1-x^2), from = -1, to = 1, add=T)
curve(-sqrt(1-x^2), from = -1, to = 1, add=T)


# matrici

# A <- matrix(x,nrow=n,ncol=m, byrow=...)
# x è un vettore di dati
# nrow e ncol sono, rispettivamente il numero di righe e di colonne
# byrow è un operatore logico. Di default è FALSE, per cui viene costruita per colonna

A <- matrix(1:8, nrow=2)
A
B <- matrix(1:8, nrow=2, byrow=TRUE)
B
C <- matrix(1:8, ncol=4, byrow=T)
C

# Matrice di 0
A0 <- matrix(0,2,3)
A0

# Matrice di 1
A1 <- matrix(1,2,3)
A1

# Riempimento senza assegnazione di valori (NA)
A2 <- matrix(,2,3)
A2

A[1,2] #estrae l'’elemento in posizione (1,2)
A[1,c(1,2)] #estrae gli elementi della prima riga
A[2,] #estrae la seconda riga
A[,1] #estrae la prima colonna

z17 <- c(1,2,3,6)
z17
z18 <- c(2,3,4,5)
z18
r_z17_z18 <- rbind(z17,z18)
r_z17_z18
c_z17_z18 <- cbind(z17,z18)
c_z17_z18
A3 <- matrix(1:8, nrow=2)
A3
A4 <- rbind(z17,A3)
A4

################# Operazioni tra matrici ###############

sample(1:6, size = 6, replace = T) # estrazione numeri con reimmissione

B1 <- matrix(sample(1:6, size = 6, replace = T), ncol=2)
B1
B2 <- matrix(sample(1:100, size = 6, replace = T), ncol=2, byrow=T)
B2

B1+B2 # somma degli elementi
B1-B2 # differenza degli elementi
B1*B2 # prodotto degli elementi
B1/B2 # divisione degli elementi

B1%*%t(B2) # prodotto matriciale
t(B1)%*%B2

solve(t(B1)%*%B2) # inversa di matrice

(t(B1)%*%B2) %*% solve(t(B1)%*%B2) # verifica

# E' possibile assegnare nomi alle righe e alle colonne di una matrice
A5 <- matrix(c(1,4,6,8,-1,-4,0,6,0),nrow=3,ncol=3,byrow=T)
A5
dimnames(A5) <- list(c("nomeriga1","nomeriga2","nomeriga3"),c("nomecol1","nomecol2","nomecol3"))

# per dare un nome solo alle righe di A5 si può usare il comando
dimnames(A5)[[1]] <- list("nomeriga1","nomeriga2","nomeriga3")

# per dare un nome solo alle colonne di A si può usare il comando
dimnames(A5)[[2]] <- list("nomecol1","nomecol2","nomecol3")


# matrici di vario genere
matrix(c(1:4), nrow=2, ncol=2, byrow = F) #square matrix	 	 	 
matrix(c(1:4), nrow=2, ncol=2, byrow = T) #square matrix	 	 	 
diag(c(1,2)) #diagonal matrix  	 	 
diag(rep(1,3)) #identity matrix	 	 	 
matrix(rep(1,times=9), nrow=3, ncol=3) # unit matrix	 ,  	 	 
var(cbind(rnorm(10), rnorm(10))) # symmetric matrix
matrix(rep(0,times=9), nrow=3, ncol=3) #null matrix	
chol(var(cbind(rnorm(10), rnorm(10)))) #upper triangular matrix


# idempotent matrix
x <- (cbind(rnorm(4), rnorm(4))) 	 		 
idemp_matr <- x %*% solve(t(x) %*% x) %*% t(x) 
idemp_matr
idemp_matr %*% idemp_matr
idemp_matr^2 # !!!
idemp_matr %*% idemp_matr %*% idemp_matr

x <- matrix(c(0.4647044, -0.8854659, 0.8854659,  0.4647044), 2,2) # orthogonal matrix	 		 
crossprod(x,x)
tcrossprod(x,x)

sum(diag(idemp_matr)) # traccia
det(idemp_matr) # determinante
det(crossprod(x,x)) # determinante

solve(idemp_matr) #### !!!!!
solve(x)


### rotazione di più vettori (trasformazioni lineari)
matr <- matrix(runif(n = 4),nrow = 2, ncol=2)
plot(matr,xlim=c(-2,2), ylim=c(-2,2))
abline(h=0, col="red")
abline(v=0)
theta <- pi/4
rot <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), nrow=2,ncol=2, byrow=T)
lines(matr%*%rot , type="p", col="red")
abline(a=0, b=sin(theta)/cos(theta), col="red")
abline(a=0, b=sin(theta+pi/2)/cos(theta++pi/2))

for(theta in seq(0,2*pi, length.out = 10)) {
  plot(matr,xlim=c(-2,2), ylim=c(-2,2))
  abline(h=0, col="red")
  abline(v=0)
  rot <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), nrow=2,ncol=2, byrow=T)
  abline(a=0, b=sin(theta)/cos(theta), col="red")
  abline(a=0, b=sin(theta+pi/2)/cos(theta++pi/2))
  
  lines(matr%*%rot , type="p", col="red")
}

# autovalori - autovettori
matr <- matrix(rnorm(100), nrow=25, ncol=4)
varcov=var(matr)
varcov
eigv <- eigen(varcov)
eigv
# ESERCIZIO ricostruire matrice matr come prodotto di matrici
C <- eigv$vectors
L <- eigv$values
C %*% diag(L) %*% t(C) 


# new matr
x <- NULL
for(i in 1:4){
  x <- cbind(x,rnorm(10))
}
svd_scomp <- svd(x)
svd_scomp
# ESERCIZIO ricostruire matrice x come prodotto di matrici
svd_scomp$u %*% diag(svd_scomp$d) %*% t(svd_scomp$v)
x
