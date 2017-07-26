########################################################
########### lezioni introduttive a R ###################
####### Analisi Statistica Multivariata 2015-2016 ######
################# PCA : addendum #######################
########################################################

library(MASS)
library(rgl)
library(heplots)

# dimensione campionaria
n <- 1000

#matr var cov
var_cov <- matrix(c(1,.99,0,
                    .99,1,0,
                    0,0,1),nrow=3, byrow=T)

# genero 3 vettori
X <- data.frame(mvrnorm(n,mu=rep(0,3),Sigma = var_cov))
colnames(X) <- c("I variab","II variab","III variab")
#genero una Y

Y <- model.matrix(~., data=X) %*% c(1,1,1,1)+rnorm(n)

#memorizzo le 3 dimensioni
x <- X[,1]
y <- X[,2]
z <- X[,3]

#estraggo le componenti principali
PCA <- eigen(var(X))

Dir_IPC <- PCA$vectors[,1] 
Dir_IIPC <- PCA$vectors[,2]
Dir_IIIPC <- PCA$vectors[,3]
Magnit_IPC <- PCA$values[1]
Magnit_IIPC <- PCA$values[2]
Magnit_IIIPC <- PCA$values[3]

#individuo le coordinate per individare la posizione degli autivettori
Coord_IPC <- PCA$vectors[,1] *PCA$values[1]
Coord_IIPC <- PCA$vectors[,2]*PCA$values[2]
Coord_IIIPC <- PCA$vectors[,3]*PCA$values[3]


#faccio un grafico 3D dei punti
par3d(FOV=0)
bg3d("gray")
plot3d(x, y, z,
       xlim=c(-5,5), xlab=names(X)[1], 
       ylim=c(-5,5), ylab=names(X)[2], 
       zlim=c(-5,5), zlab=names(X)[3],
        main="scatterplot3d", pch=1, size =.001)

#aggiungo le direzioni degli autovettori
arrow3d(c(0,0,0),Coord_IPC, barblen=.2, lwd=5, col="red")
arrow3d(c(0,0,0),Coord_IIPC, barblen=.2, lwd=5, col="red")
arrow3d(c(0,0,0),Coord_IIIPC, barblen=.2, lwd=5, col="red")

#disegno l'ellissoide descritto dalla forma quadratica nella gaussiana 
plot3d(ellipse3d(var_cov),
       xlim=c(-5,5), xlab=names(X)[1], 
       ylim=c(-5,5), ylab=names(X)[2], 
       zlim=c(-5,5), zlab=names(X)[3],
       col = "green")

#chiudo i grafici
rgl.close()

# confronto la regressione usando X vs utilizzo dei punteggi delle prime 2 PC

PC <- data.frame(as.matrix(X) %*% (PCA$vectors))
colnames(PC) <- c("I_PC","II_PC","III_PC")

summary(lm(Y~., data=X))
summary(lm(Y~I_PC+II_PC, data=PC))
