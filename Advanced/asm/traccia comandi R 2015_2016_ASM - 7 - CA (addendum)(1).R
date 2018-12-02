########################################################
########### lezioni introduttive a R ###################
####### Analisi Statistica Multivariata 2015-2016 ######
################# CA : addendum #######################
########################################################

library(MASS)
library(rgl)
library(heplots)


#graphics.off()
#rm(list=ls(all=TRUE))

x <- (read.table("clipboard", sep="\t", header=T))
x

# utile solo se x è strutturato come data frame
# rownames(x) <- x[,1]
#x[,1] <- NULL
# x

# qui dipende dall'input
# creare una table (se non è già disponibile)
# x <- (table(x))
#####
#### se x non ha struttura di tabella ( o data frame, ripetere input) !!!!


#x=x[,3:(ncol(x)-2)]
xrows=row.names(x)
xcols=colnames(x)

a = apply(x,1,sum)  # marginali di riga
b = apply(x,2,sum)  # marginali di colonna
e = a%*%t(b)/sum(a) # frequenze "teoriche"

C = (x-e)/sqrt(e)   # chi-matrix
sum(C^2)              # valore del chi-quadro
sum(C^2)/(sum(x)*(min(dim(x))-1))              # valore del chi-quadro normalizzato

Y = svd(C)		      # svd
g = Y$u 
l = Y$d
v = Y$v               

# eigenvalues
ll = l^2

## percentuale spiegata
ll # notare che l'ultimo autovalore è sempre nullo perché sussiste dip lineare 
per = cumsum(l^2)/sum(l^2)
per  

# rappresentazione della scomposizione di chi pesata per riga
r = (matrix(l,nrow(g),ncol(g),byrow=TRUE)*g)/matrix(sqrt(a),nrow(g),ncol(g),byrow=FALSE)

# rappresentazione della scomposizione di chi pesata per colonna
s = (matrix(l,nrow(v),ncol(v),byrow=TRUE)*v)/matrix(sqrt(b),nrow(v),ncol(v),byrow=FALSE)

#  r[,1:2]  #proiezione riga sulle prime due comp
#  s[,1:2]  #proiezione colonna sulle prime due comp

#####################################################
# se la dim minima delle categorie è 2
#####################################################
if (min(dim(x))==2){
  rr=r[,1]
  ss=s[,1]
  #rr[,2] = rr[,2]*sign(rr[1,2])
  #ss[,2] = ss[,2]*sign(ss[1,2])
  
  # confronto del piano fattoriale con le sole prime due componenti
  par(mfrow=c(1,1))
  # 2 fattori 1vs 2
  plot(rr,
       xlim=c(min(rr),max(rr)),
       ylim=c(-.1,.1),
       type="n",
       xlab=expression(list(r[1],s[1])),main="piano fattoriale")
  text(ss,0,xcols,cex=1,col="red",xpd=NA)
  text(rr,0,xrows,cex=1,col="black",xpd=NA)
  abline(v=0,lwd=1)
  
} 

#####################################################
# se la dim minima delle categorie è 3
#####################################################
if (min(dim(x))==3){
  rr=r[,1:2]
  ss=s[,1:2]
  #rr[,2] = rr[,2]*sign(rr[1,2])
  #ss[,2] = ss[,2]*sign(ss[1,2])
  
  # confronto dei piani fattoriali a coppie
  par(mfrow=c(1,1))
  # 2 fattori 1vs 2
  plot(rr[,1],rr[,2],
       xlim=c(min(rr,ss),max(rr,ss)),
       ylim=c(min(rr,ss),max(rr,ss)),
       type="n",
       xlab=expression(list(r[1],s[1])),ylab=expression(list(r[2],s[2])),main="car marks data")
  points(ss[,1],ss[,2],type="n")
  text(ss[,1],ss[,2],xcols,cex=1,col="red",xpd=NA)
  text(rr[,1],rr[,2],xrows,cex=1,col="black",xpd=NA)
  abline(h=0,v=0,lwd=2)
  
} else {
  #####################################################
  # se la dim minima delle categorie è di almeno 3
  #####################################################
   
rr=r[,1:3]
ss=s[,1:3]
#rr[,2] = rr[,2]*sign(rr[1,2])
#ss[,2] = ss[,2]*sign(ss[1,2])

# confronto dei piani fattoriali a coppie
par(mfrow=c(1,3))
# 2 fattori 1vs 2
plot(rr[,1],rr[,2],
     xlim=c(min(rr,ss),max(rr,ss)),
     ylim=c(min(rr,ss),max(rr,ss)),
     type="n",
     xlab=expression(list(r[1],s[1])),ylab=expression(list(r[2],s[2])),main="car marks data")
points(ss[,1],ss[,2],type="n")
text(ss[,1],ss[,2],xcols,cex=1,col="red",xpd=NA)
text(rr[,1],rr[,2],xrows,cex=1,col="black",xpd=NA)
abline(h=0,v=0,lwd=2)

# 2 fattori 1 vs 3
plot(rr[,1],rr[,3],
     xlim=c(min(rr,ss),max(rr,ss)),
     ylim=c(min(rr,ss),max(rr,ss)),
     type="n",
     xlab=expression(list(r[1],s[1])),ylab=expression(list(r[3],s[3])),main="car marks data")
points(ss[,1],ss[,3],type="n")
text(ss[,1],ss[,3],xcols,cex=1,col="red",xpd=NA)
text(rr[,1],rr[,3],xrows,cex=1,col="black",xpd=NA)
abline(h=0,v=0,lwd=2)

# 2 fattori 2 vs 3
plot(rr[,1],rr[,2],
     xlim=c(min(rr,ss),max(rr,ss)),
     ylim=c(min(rr,ss),max(rr,ss)),
     type="n",
     xlab=expression(list(r[2],s[2])),ylab=expression(list(r[3],s[3])),main="car marks data")
points(ss[,2],ss[,3],type="n")
text(ss[,2],ss[,3],xcols,cex=1,col="red",xpd=NA)
text(rr[,2],rr[,3],xrows,cex=1,col="black",xpd=NA)
abline(h=0,v=0,lwd=2)

# confronto dei primi 3 piani fattoriali congiuntamente presi
#faccio un grafico 3D dei punti
par3d(FOV=0)
bg3d("gray")
plot3d(c(rr[,1],ss[,1]),c(rr[,2],ss[,2]),c(rr[,3],ss[,3]),
       xlim=c(min(rr,ss),max(rr,ss)), 
       ylim=c(min(rr,ss),max(rr,ss)), 
       zlim=c(min(rr,ss),max(rr,ss)), 
       xlab="r_1,s_1",
       ylab="r_2,s_2",
       zlab="r_3,s_3",
       main="scatterplot3d", cex=.25,pch=1, size =3)

#aggiungo le etichette 
text3d(c(rr[,1],ss[,1]),c(rr[,2],ss[,2]),c(rr[,3],ss[,3]),
       c(xrows, xcols),cex=1,
       col=c(rep("red",length(xrows)),rep("blue",length(xcols))),
       xpd=NA)

#aggiungo gli assi 
axes3d('x', po=c(NA,0,0), cex=.5)
axes3d('y', po=c(0,NA,0), cex=.5)
axes3d('z', po=c(0,0,NA), cex=.5)

#chiudo il grafico
# rgl.close()
}
