########################################################
########### lezioni introduttive a R ###################
####### Analisi Statistica Multivariata 2015-2016 ######
##################### Cluster  #########################
########################################################

# esempio di misure di prossimità

# si consideri il vettore
x <- c(1,2,4,8)
x <- as.data.frame(x)
row.names(x) <- t(x)

#questa funzione serve per definire diversi tipi di distanze
?dist
dist(x)

dist(x, "euclidean")
dist(x, "maximum")
dist(x, "manhattan")
dist(x, "canberra")
dist(x, "minkowski", p=2)

# si consideri la matrice
x <- c(1,2,4,8)
y <- c(2,3,5,9)
X <- cbind(x,y)
X <- as.data.frame(X)
row.names(X) <- 1:dim(X)[1]

dist(X)

dist(X, "euclidean")
dist(X, "maximum")
dist(X, "manhattan")
dist(X, "canberra")
dist(X, "minkowski", p=2)

##### algoritmi di aggregazione gerarchica
?hclust

##### single linkage
hclust(dist(x), method = "single")

##### dendrogramma
?dendrogram
as.dendrogram(hclust(dist(x), method = "single"))
plot(as.dendrogram(hclust(dist(x), method = "single")))

# o anche 
plot(hclust(dist(x), method = "single"),hang = -1)

##### complete linkage
hclust(dist(x), method = "complete")
as.dendrogram(hclust(dist(x), method = "complete"))
plot(as.dendrogram(hclust(dist(x), method = "complete")))


##### ward linkage
hclust(dist(x), method = "ward.D")
as.dendrogram(hclust(dist(x), method = "ward.D"))
plot(as.dendrogram(hclust(dist(x), method = "ward.D")))

# confronto tra metodi
x <- sample(1:1000,size = 20)
par(mfrow=c(1,3))
plot(as.dendrogram(hclust(dist(x), method = "single")))
plot(as.dendrogram(hclust(dist(x), method = "complete")))
plot(as.dendrogram(hclust(dist(x), method = "ward")))

#### comando cutree
cutree(hclust(dist(x), "single"), k =2 )
cutree(hclust(dist(x), "complete"), k =2 )
cutree(hclust(dist(x), "ward.D"), k =2 )

### codice complicato ....
par(mfrow=c(1,1))

x <- sample(1:1000,size = 1000)

x <- as.data.frame(x)
row.names(x) <- t(x)
distSamples <- dist(x)
hc <- hclust(distSamples)

# valido per k<=11 gruppi...
labelColors = c("red", "green", "blue", 
                "yellow", "brown", "magenta",
                "black", "lighblue", "pink", "gray",
                "skyblue")

# cut dendrogramma in 5 clusters
clusMember = cutree(hc,  5)

## funzione per settaggio dei colori
labelCol <- function(y) {
  if (is.leaf(y)) {
    a <- attributes(y)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(y, "nodePar") <- c(a$nodePar, lab.col = labCol)  }
  return(y)
}
## dendrapply sui nodi del dendrogram
d <- dendrapply(as.dendrogram(hc), labelCol)
plot(d)


##### algoritmo k-means per aggregazione non gerarchica
?kmeans

# si consideri la matrice
x <- c(1,2,4,8)
y <- c(2,3,5,9)
X <- cbind(x,y)
X <- as.data.frame(X)
row.names(X) <- 1:dim(X)[1]

cl <- kmeans(X, 2)
plot(X, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)


