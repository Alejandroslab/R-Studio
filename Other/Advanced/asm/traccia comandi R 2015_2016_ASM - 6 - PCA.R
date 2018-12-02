########################################################
########### lezioni introduttive a R ###################
####### Analisi Statistica Multivariata 2015-2016 ######
###################### PCA #############################
########################################################

rm(list=ls())
library(MASS)

# esempio simulato con 2 variabili
set.seed(123)

var_cov <- matrix(c(1,.9,
                    .9,1), nrow=2, byrow=T)
var_cov

x <- mvrnorm(n = 100,mu = c(0,0),Sigma = var_cov)
var(x)

x <- scale(x);cor(x)

n <- nrow(x);

tmp <- eigen(cov(x), symmetric = TRUE);
str(tmp)
e <- tmp$values;
v <- tmp$vectors;                 # spectral analysis
tmp <- cbind(e,t(v))[order(-e),]; # ordino la matrice in base alla magnitudo degli autovalori

e <- tmp[,1]; # memorizzo gli autovalori ordinati
v <- t(tmp[,2:3]); # memorizzo gli autovettori ordinati in base agli autovalori

print("autovettori e percentuale di variabilità spiegata");
print(t(cbind(autovl=e,perc_spieg=(e/sum(e)),perc_spieg_cum=(cumsum(e)/sum(e)))));
print("2 autovettori") # primi 2 autovalori
print(t(v[,1:2]));

y <- x %*% v;   # principal components (se si vuole la controrotazione usare -v)
x12 <- y[,1:2];

op <- par(mfrow = c(1,2));
plot(x, xlim=c(-4,4), ylim=c(-4,4),main = "I  vs. II PC", col="red");
lines(x12, type="p",xlim=c(-4,4), ylim=c(-4,4),main = "I  vs. II PC", xlab = "pc1", ylab = "pc2");

# screeplot of eigenvalues
plot(cbind(c(1:2),e), type="l",main = "eigenvalues of S", xlab = "", ylab = expression(lambda));
par(mfrow = c(1,1))

# correlazione tra pc e variabili
r <- cor(cbind(y,x));r
t(t(v) *sqrt(e)) # correlazione tra prime 2 Pc e variabili
r <- r[3:4,1:2];r  # equivalente

#grafico delle correlazioni
ucircle <- cbind(cos((0:360)/180*3.14159),sin((0:360)/180*3.14159));
xline <- cbind(c(-1.1,1.1),c(0,0));
yline <- cbind(c(0,0),c(-1.1,1.1));
plot(ucircle, type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
lines(xline, lwd = 2);
lines(yline, lwd = 2);
points(r);
text(r, c("X1","X2"), adj = 1.2);
arrows(x0 = 0,y0 = 0,x1 = r[,1],y1 =r[,2], col="red")
par(op);

par(mfrow=c(1,1))
# biplot (un esempio)
#grafico delle correlazioni
plot(ucircle, ylim=c(-5,5), xlim=c(-5,5),type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
points(r*2);
text(r*2, c("X1","X2"), adj = 1.2);
arrows(x0 = 0,y0 = 0,x1 = r[,1]*2,y1 =r[,2]*2, col="red")
lines(x12, type="p", cex=.25)
text(x12, as.character(seq(1:n)), cex=.5,adj = 1.2);
par(op);


### in alternativa !!!!!!
dev.new()
x <- data.frame(x)
X_PC <- prcomp(~ .,scale.=T,data=x)
biplot(X_PC,cex=0.5)


##################################################################
# esempio simulato con 4 variabili
set.seed(123)

var_cov <- matrix(c(1,.9,-.5,0,
                  .9,1,-.5,0,
                  -.5,-.5,1,0,
                  0,0,0,1), nrow=4, byrow=T)
var_cov

x <- mvrnorm(n = 100,mu = c(0,0,0,0),Sigma = var_cov)
var(x)

x <- scale(x);cor(x)

n <- nrow(x);

tmp <- eigen(cov(x), symmetric = TRUE);
str(tmp)
e <- tmp$values;
v <- tmp$vectors;                 # spectral analysis
tmp <- cbind(e,t(v))[order(-e),]; # ordino la matrice in base alla magnitudo degli autovalori

e <- tmp[,1];
v <- t(tmp[,2:5]);

print("autovettori e percentuale di variabilità spiegata");
print(t(cbind(autovl=e,perc_spieg=(e/sum(e)),perc_spieg_cum=(cumsum(e)/sum(e)))));
print("2 autovettori") # primi 2 autovalori
print(t(v[,1:2]));

y <- x %*% v;                     # principal components
x12 <- y[,1:2];

op <- par(mfrow = c(1,2));
plot(x12, main = "I  vs. II PC", xlab = "pc1", ylab = "pc2");

# screeplot of eigenvalues
plot(cbind(c(1:4),e), type="l",main = "eigenvalues of S", xlab = "", ylab = expression(lambda));
par(mfrow = c(1,1))

# correlazione tra pc e variabili
r <- cor(cbind(y,x));r
t(t(v) *sqrt(e)) # correlazione tra prime 2 Pc e variabili
r <- r[5:8,1:2];r  # equivalente

#grafico delle correlazioni
ucircle <- cbind(cos((0:360)/180*3.14159),sin((0:360)/180*3.14159));
xline <- cbind(c(-1.1,1.1),c(0,0));
yline <- cbind(c(0,0),c(-1.1,1.1));
plot(ucircle, type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
lines(xline, lwd = 2);
lines(yline, lwd = 2);
points(r);
text(r, c("X1","X2","X3","X4"), adj = 1.2);
arrows(x0 = 0,y0 = 0,x1 = r[,1],y1 =r[,2], col="red")
par(op);


# biplot (un esempio)
#grafico delle correlazioni
plot(ucircle, ylim=c(-5,5), xlim=c(-5,5),type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
points(r*2);
text(r*2, c("X1","X2","X3","X4"), adj = 1.2);
arrows(x0 = 0,y0 = 0,x1 = r[,1]*2,y1 =r[,2]*2, col="red")
lines(x12, type="p", cex=.25)
text(x12, as.character(seq(1:n)), cex=.5,adj = 1.2);
par(op);


### in alternativa !!!!!!
dev.new()
x <- data.frame(x)
X_PC <- prcomp(~ .,scale.=T,data=x)
biplot(X_PC,cex=0.5)



##################################################################

# esempio simulato: caso di correlazione esatta
u <- matrix(runif(100), ncol = 2);
a <- cbind(c(1,0), c(0,1), c(1,1), c(1, -1));
x <- u %*% a;                     # generazione data base
cor(x)

x <- scale(x);
cor(x)
n <- nrow(x);
tmp <- eigen(cov(x), symmetric = TRUE);
e <- tmp$values;
v <- tmp$vectors;                 # spectral analysis
tmp <- cbind(e,t(v))[order(-e),];
e <- tmp[,1];
v <- t(tmp[,2:5]);

print("autovettori e percentuale di variabilità spiegata");
print(t(cbind(autovl=e,perc_spieg=(e/sum(e)),perc_spieg_cum=(cumsum(e)/sum(e)))));
print("2 autovettori") # primi 2 autovalori
print(t(v[,1:2]));

y <- x %*% v;                     # principal components
x12 <- y[,1:2];

op <- par(mfrow = c(1,2));
plot(x12, main = "I  vs. II PC", xlab = "pc1", ylab = "pc2");

# screeplot of eigenvalues
plot(cbind(c(1:4),e), type="l",main = "eigenvalues of S", xlab = "", ylab = expression(lambda));
par(mfrow = c(1,1))

# correlazione tra pc e variabili
r <- cor(cbind(y,x));
t(t(v) *sqrt(e))
r <- r[5:8,1:2];                  # correlazione tra prime 2 Pc e variabili

#grafico delle correlazioni
ucircle <- cbind(cos((0:360)/180*3.14159),sin((0:360)/180*3.14159));
xline <- cbind(c(-1.1,1.1),c(0,0));
yline <- cbind(c(0,0),c(-1.1,1.1));
plot(ucircle, type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
lines(xline, lwd = 2);
lines(yline, lwd = 2);
points(r);
text(r, c("X1","X2","X3","X4"), adj = 1.2);
arrows(x0 = 0,y0 = 0,x1 = r[,1],y1 =r[,2], col="red")
par(op);


# biplot (un esempio)
#grafico delle correlazioni
plot(ucircle, ylim=c(-5,5), xlim=c(-5,5),type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
points(r*2);
text(r*2, c("X1","X2","X3","X4"), adj = 1.2);
arrows(x0 = 0,y0 = 0,x1 = r[,1]*2,y1 =r[,2]*2, col="red")
lines(x12, type="p", cex=.25)
text(x12, as.character(seq(1:n)), cex=.5,adj = 1.2);
par(op);



### in alternativa !!!!!!
x <- data.frame(x)
X_PC <- prcomp(~ .,scale.=T,data=x)
biplot(X_PC)

######################################
# distribuzione (simulata) degli autovalori
######################################

# esempio simulato
set.seed(123)

var_cov <- matrix(c(1,.9,-.5,0,
                  .9,1,-.5,0,
                  -.5,-.5,1,0,
                  0,0,0,1), nrow=4, byrow=T)

eigenv_simu <- NULL
eigenv_perc_spieg <- NULL
for(i in 1:1000){
	x <- mvrnorm(n = 1000,mu = c(0,0,0,0),Sigma = var_cov)
	x <- scale(x);cor(x)
	n <- nrow(x);
	tmp <- eigen(cov(x), symmetric = TRUE);
	e <- tmp$values;
	v <- tmp$vectors;                 # spectral analysis
	tmp <- cbind(e,t(v))[order(-e),]; # ordino la matrice in base alla magnitudo degli autovalori
	e <- tmp[,1];
	v <- t(tmp[,2:5]);
	eigenv_simu <- rbind(eigenv_simu,autovl=e) 
	eigenv_perc_spieg <- rbind(eigenv_perc_spieg , perc_spieg_cum=(cumsum(e)/sum(e)))
}

hist(eigenv_simu[,1])
hist(eigenv_perc_spieg[,2])


###################################
### third party claims
##################################

rm(list=ls())
load(file="tpc.rdata") # caricamento file in formato Rdata

rownames(tpc) <- tpc$lga # assegnazione dei nomi di riga in base al campo lga

tpc$lga <- NULL # cancellazione del campo lga

# standardizzazione variabili (vedi anche scale)
mat <- as.matrix(tpc[,-(1:2)]) #### escludo sd e claims !!!

for (i in 1:4) mat[,i] = (mat[,i]-mean(mat[,i]))/sqrt(var(mat[,i])) # standardizzo (se necessario)

pc <- prcomp(mat)
pc

pc1 <- mat%*%pc$rotation[,1]
pc2 <- mat%*%pc$rotation[,2]
pc3 <- mat%*%pc$rotation[,3]


#op <- par(mfrow = c(2,2),ask = dev.interactive(orNone = F))
contee <- row.names(tpc)
plot(pc2~pc1, main="I vs. II PC", type="n")
text(pc1,pc2,tpc$sd,xpd=NA,cex=0.5,col=tpc$sd)
text(pc1,pc2,contee,xpd=NA,cex=0.5,col=tpc$sd)
plot(pc3~pc2, main="II vs. III PC", type="n")
text(pc2,pc3,tpc$sd,xpd=NA,cex=0.5,col=tpc$sd)
text(pc2,pc3,contee,xpd=NA,cex=0.5,col=tpc$sd)
plot(pc3~pc1, main="I vs. III PC", type="n")
text(pc1,pc3,tpc$sd,xpd=NA,cex=0.5,col=tpc$sd)
text(pc1,pc3,contee,xpd=NA,cex=0.5,col=tpc$sd)
eig <- eigen(cov(mat))$values
print(t(cbind(eig,(eig/sum(eig)),(cumsum(eig)/sum(eig)))));
plot(eig, type="l",main = "eigenvalues of S",xlab="",ylab=expression(lambda))
par(op)

# correlazioni
cov1 <- pc$rotation%*%diag(pc$sdev)
r  <- cov1[,1:2]

par(mfrow = c(1,1))
ucircle <- cbind(cos((0:360)/180*3.14159),sin((0:360)/180*3.14159));
xline <- cbind(c(-1.1,1.1),c(0,0));
yline <- cbind(c(0,0),c(-1.1,1.1));
plot(ucircle, type = "n", main = "Tpc", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
lines(xline, lwd = 2);
lines(yline, lwd = 2);
points(r);
text(r,rownames(cov1), adj = .5);
arrows(x0 = 0,y0 = 0,x1 = r[,1],y1 =r[,2], col="red")
par(op);


# biplot (un esempio)
#grafico delle correlazioni
plot(ucircle, ylim=c(-5,5), xlim=c(-5,5),type = "n", main = "data set simulato", xlab = "I PC", ylab = "II PC");
lines(ucircle, lty = "dashed");
points(r*2);
text(r*2, rownames(cov1), adj = .5);
arrows(x0 = 0,y0 = 0,x1 = r[,1]*2,y1 =r[,2]*2, col="red")

x12 <- mat%*%pc$rotation[,1:2]
lines(pc1,pc2, type="p", cex=.3,col=tpc$sd)
text(pc1,pc2,,tpc$sd,xpd=NA,cex=0.5,col=tpc$sd)
text(pc1,pc2,contee,xpd=NA,cex=0.25,col=tpc$sd)

#text(x12, rownames(mat), cex=.5,adj = 1.2);
par(op);

print("autovalori")
print(eigen(cov(mat))$values)
print("I pc")
print(pc$rotation[,1])
print("II pc")
print(pc$rotation[,2])
par(op)


### in alternativa !!!!!!
x <- tpc[,-(1:2)]
X_PC <- prcomp(~ .,scale.=T,data=x)
biplot(X_PC, cex=.3)


###### confronto regressione con variabili originarie vs uso delle solo prime 2 comp principali (non standardizzate)

X_PC <- prcomp(~ .,scale.=F,data=tpc[,-(1:2)])
IPC <-  apply(tpc[,-(1:2)] * X_PC$rotation[,1], MARGIN=1, sum)
IIPC <- apply(tpc[,-(1:2)] * X_PC$rotation[,2], MARGIN=1, sum)
tpc <- data.frame(tpc, IPC=IPC, IIPC=IIPC)
summary(lm (claims~as.factor(sd)+IPC+IIPC, data=tpc))
summary(lm (claims~as.factor(sd)+accidents+ki+population+pop_density, data=tpc))



