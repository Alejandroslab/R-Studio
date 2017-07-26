rm(list=ls())
tpc <- read.table("clipboard", head=T, dec=".") # per caricamento diretto da excel ("copia/incolla")

head(tpc) # controllo della coerenza del caricamento
str(tpc) # controllo della coerenza del caricamento

# save(file="tpc.rdata", tpc) # salvataggio in formato Rdata
# load(file="tpc.rdata") # caricamento file in formato Rdata

rownames(tpc) <- tpc$lga # assegnazione dei nomi di riga in base al campo lga

tpc$lga <- NULL # cancellazione del campo lga

head(tpc)

summary(tpc) # statistiche descrittive
cor(tpc) # matr correlazione

det(solve(crossprod(as.matrix(tpc),as.matrix(tpc)))) # !!!!!! quasi singolarità

pairs(tpc) # scatterplot matrix

par(mfrow=c(1,1))
plot(tpc$accidents, tpc$claims) # verifica coerenza dei campi claims <= accidents
abline(a=0, b=1)

with(tpc, plot(accidents, claims)) # stesso grafico
abline(a=0, b=1, col= "red", lty=2)

# quali record sono incoerenti?
anomali <- with(tpc, which(accidents<claims)) # stesso grafico
tpc[anomali,]

# filtro su incoerenti
boolean <- with(tpc, (accidents>claims)) # stesso grafico

# regressione full regressors
summary(lm(claims ~ . , data=tpc))

# regressione full regressors con filtro
summary(lm(claims ~ . , data=tpc, subset=boolean))

#diagnostiche
par(mfrow=c(2,2))
plot((lm(claims ~ . , data=tpc)))

# quali record sono incoerenti?
anomali <- with(tpc, which(rownames(tpc)=="FAIRFIELD")) # stesso grafico
tpc[anomali,]

# filtro su incoerenti
boolean <-  boolean & with(tpc, (rownames(tpc)!="FAIRFIELD")) # stesso grafico

# regressione full regressors con filtro
summary(lm(claims ~ . , data=tpc, subset=boolean))

#diagnostiche
par(mfrow=c(2,2))
plot((lm(claims ~ . , data=tpc, subset=boolean)))

# ETC....

# creazione di nuovo campo
tpc <- with(tpc,  data.frame(tpc, ki_acc = ki/accidents))
tpc <- with(tpc,  data.frame(tpc, ki_pop = ki/population))
tpc <- with(tpc,  data.frame(tpc, acc_pop = accidents/population))

# nuova regressione
new1 <- lm(claims ~ . , data=tpc)
summary(new1)
plot(new1)

# tolgo un regressore
fm1<-update(new1, . ~ .-ki)
summary(fm1)
plot(fm1)

# tolgo un regressore
fm1<-update(new1, . ~ .-accidents)
summary(fm1)
plot(fm1)

# tolgo due regressori
fm1<-update(new1, . ~ .-ki-population)
summary(fm1)
plot(fm1)

fm1<-update(new1, . ~ .-ki-population-pop_density)
summary(fm1)
plot(fm1)

fm1<-update(new1, . ~ .-ki-population-pop_density-accidents)
summary(fm1)
plot(fm1)

pairs(tpc)

#### RUOLO DI sd

tpc$sd <- as.factor(tpc$sd) 


