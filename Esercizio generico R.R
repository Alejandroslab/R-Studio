a<-c(1,2,3,4,5,6,7,8,9,10)
sum(a)
summary(a)
plot(a)
help()
#varianza
var(a)
#dataset alberi altezza, girth e volume di 31 alberi diversi
trees
summary(trees)
plot(trees)
help(trees)
plot(volcano)
require(graphics)
volcano
k<-volcano
library(foreign)
help(foreign)
??foreign
#per fare una sequenza di numeri si utilizza seq 
sequenzanumeri<-seq(1,10)
sum(seq(1,8))
#per fattoriale 3*2*1
factorial(3)
#nuovo dataframe, selezionare tutto il codice e dare invio
dataframe1<-data.frame(
  x=seq(0,4),
  y=seq(5,9),
  z=seq(10,14)
)
x1 = c(50000.92,49998.70, 49998.89,50000.47)
50000.92+49998.70+49998.89+50000.47
x2<-(double)
k
plot(k)
175+1.645*4
help("plot")
install.packages("scatterplot3d")
install.packages("AER")

## example 1
z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue", main="scatterplot3d - 1", pch=20)

help("AER")
??AER
data(package = "AER")
Affairs
y
CASchools
tree
trees
y
plot(12*sequenzanumeri*(1-sequenzanumeri)^2)
plot(1/sqrt(2)*dataframe1)
#Simulazione in R di una variabile casuale doppia.
#Supponiamo che la variabile doppia (X, Y ) sia tale che Y |X = x ⇠ N(x, 1) e
#X ⇠ N(0, 1). Vogliamo disegnare in R 1000 realizzazioni della variabile doppia
#(X, Y ). Possiamo procedere cosi

x=rnorm(1000,0,1)
y=rnorm(1000,x,1)
plot(x=x,y=y,pch=16,col=2)

#per istogramma
hist(x)

Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
var(mvrnorm(n = 1000, rep(0, 2), Sigma))
var(mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE))
plot(mvrnorm(n=2,Sigma = 2))
#example

dmvnorm(x=c(0,0))
dmvnorm(x=c(0,0), mean=c(1,1))
sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
colMeans(x)
var(x)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
colMeans(x)
var(x)
plot(x)

rbivariate <- function(mean.x = 70, sd.x=3, mean.y=162, sd.y=14, r=.50, iter=100) {
  z1 <- rnorm(iter)
  z2 <- rnorm(iter)
  x <- sqrt(1-r^2)*sd.x*z1 + r*sd.x*z2 + mean.x
  y <- sd.y*z2 + mean.y
  return(list(x,y))
}

data <- rbivariate(iter=1000)
mean(data[[1]])
sd(data[[1]])
mean(data[[2]])
sd(data[[2]])
plot(data[[1]],data[[2]])

install.packages("devtools")
help("survival-internal")

temp <- seq(-pi, 0, length = 50)
x <- c(rep(1, 50) %*% t(cos(temp)))
y <- c(cos(temp) %*% t(sin(temp)))
z <- c(sin(temp) %*% t(sin(temp)))
scatterplot3d(x, y, z, highlight.3d = TRUE, angle = 120,
              col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
              cex.lab = 1.1, main = "Hemisphere", pch = 20)

#Bivariate normal distribution

library("mvtnorm")
x1 <- x2 <- seq(-10, 10, length = 51)
dens <- matrix(dmvnorm(expand.grid(x1, x2),
                       sigma = rbind(c(3, 2), c(2, 3))),
               ncol = length(x1))
s3d <- scatterplot3d(x1, x2,
                     seq(min(dens), max(dens), length = length(x1)),
                     type = "n", grid = FALSE, angle = 70,
                     zlab = expression(f(x[1], x[2])),
                     xlab = expression(x[1]), ylab = expression(x[2]),
                     main = "Bivariate normal distribution")
text(s3d$xyz.convert(-1, 10, 0.07),
     labels = expression(f(x) == frac(1, sqrt((2 * pi)^n *
                                                phantom(".") * det(Sigma[X]))) * phantom(".") * exp * {
                                                  bgroup("(", - scriptstyle(frac(1, 2) * phantom(".")) *
                                                           (x - mu)^T * Sigma[X]^-1 * (x - mu), ")")}))
text(s3d$xyz.convert(1.5, 10, 0.05),
     labels = expression("with" * phantom("m") *
                           mu == bgroup("(", atop(0, 0), ")") * phantom(".") * "," *
                           phantom(0) *
                           {Sigma[X] == bgroup("(", atop(3 * phantom(0) * 2,
                                                         2 * phantom(0) * 3), ")")}))
for(i in length(x1):1)
  s3d$points3d(rep(x1[i], length(x2)), x2, dens[i,], type = "l")
for(i in length(x2):1)
  s3d$points3d(x1, rep(x2[i], length(x1)), dens[,i], type = "l")

help("persp")

par(bg = "white")
x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
z <- outer(x, y, function(a, b) a*b^2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 50
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)

par(op)
install.packages("rggobi")

data(PaulKAI)
rownames(PaulKAI) # the ten epistles researched:
##> [1] "Rom" "Co1" "Co2" "Gal" "Phi" "Col" "Th1" "Ti1" "Ti2" "Heb"
PaulKAI # the 10 x 4 count table
mosaicplot(PaulKAI)
quadplot(PaulKAI)
