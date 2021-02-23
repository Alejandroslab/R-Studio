summary(AAPL_Historical_Data)
plot(AAPL_Historical_Data$Price)

rm(AAPL_Historical_Data)


Apple


summary(AAPL_Historical_Data$Price)

i<-c(1:5000)

Apple2<- cbind(AAPL_Historical_Data, i)

Apple<-Apple2[order(Apple2$i, decreasing = TRUE),]



plot(Apple$Price, type = "l")


summary(Apple)

tail(Apple)

(Apple$Price[5000]-Apple$Price[1])/Apple$Price[1] *100

logcompoundreturns<-log(diff(Apple$`Change %`))

plot(logcompoundreturns)

summary(logcompoundreturns)



portpirie_asmatrix<-matrix(portpirie)


plot(portpirie)


logcompoundreturns_gev<- fgev(logcompoundreturns)


logcompoundreturns_matr<-matrix(logcompoundreturns)


portpirie_fgev<- fgev(portpirie)


#the log compound return is wrong as the formula is log(Xn/ X(n-1) and not log(Xn - X(n-1))

#so I have to find a lagged ratio function or create one 

#as for example create something like this

lagged_ratio<- function(x, lag=1L){
  
  xlagged <- xlagged[,n]/xlagged[,n+1]
  
  xlagged

  
}


lagged_ratio(portpirie)



Return.calculate(Apple$Price)










fitted(portpirie_fgev)

confint(portpirie_fgev, level=0.95)


ED2<- cbind (ED_Historical_Data, i)

ED<-ED2[order(ED2$i, decreasing = TRUE),]


plot(i,Apple$Price, ylab = "Apple")

lines(ED$Price, type = "l")

(ED$Price[5000]-ED$Price[1])/ED$Price[1] *100


summary(Apple$Price)
lines(ED$Price)




summary(Apple$`Change %`)
grep(-51.83, Apple$`Change %`)

Apple[468, ]



variazione_perc <- function(x,y){
  variazione_perc = (y-x)/x *100
  variazione_perc
  
}




AGL2<- cbind (AGL_Historical_Data, i)
AGL<-AGL2[order(AGL2$i, decreasing = TRUE),]
AGL

lines(AGL$Price)

var(AGL$Price)


cov(Apple$Price, ED$Price)

variazione_perc(AGL$Price[1], AGL$Price[5000])


variazione_perc(ED$Price[1], ED$Price[5000])

variazione_perc(Apple$Price[1], Apple$Price[5000])

summary(AGL$`Change %`)

min(AGL$`Change %`)
min(Apple$`Change %`)

minimumchangesperday<-min(Apple$`Change %`)

minimumchangesperday<-rbind(minimumchangesperday,min(ED$`Change %`) )

summary(minimumchangesperday)


cov(Apple$`Change %`,ED$`Change %`)


var(Apple$`Change %`)

VaR(try, p=0.95, method = "gaussian", clean = "none", portfolio_method = "single")

try<-c(21000,21200,21300,21400,21000)



mean3stock_20years<-mean(variazione_perc(AGL$Price[1], AGL$Price[5000]),
     variazione_perc(ED$Price[1], ED$Price[5000]),
     
     variazione_perc(Apple$Price[1], Apple$Price[5000]))

#backtesting on the profit on a mean of the 3 stocks last 20 years returns

totalassetsnow<- 23000

mean3stock_20years/100*totalassetsnow

#if I would have only apple, cons edison and autogrill after seeing the past data the potential profit will be 200% in 20 years (without considering the dividends)


#therefore my assets would become 46151.32 euro.

#the minimum loss precentage change in a single day was -51.3%

#maximum drawdown would be the weighted mean of the maximum loss in a single day that is -50%

summary(Apple$`Change %`)
grep(-51.83, Apple)

min(Apple$`Change %`)


grep(-51.830, Apple$`Change %`)

Apple[468,]

variazione_perc(2.01,1.84)



variazione_perc(14.95,45.16)








file_list=list.files(path="Library/Mobile Documents/com~apple~CloudDocs/Formazione Aggiuntiva/STATISTICA/DATA/Portfolio Data")
no_files<-length(file_list)
i=1
for (i in 1:no_files){
  tempname<-file_list[i]
  tempdataset<-read.csv(paste("Library/Mobile Documents/com~apple~CloudDocs/Formazione Aggiuntiva/STATISTICA/DATA/Portfolio Data",tempname,sep=""))
  tempdataset<- tempdataset[seq(dim(tempdataset)[1],1),]
  write.csv(tempdataset,paste("Library/Mobile Documents/com~apple~CloudDocs/Formazione Aggiuntiva/STATISTICA/DATA/Portfolio Data/sorted",tempname,sep=""))
  
}



folder<-"~/Library/Mobile Documents/com~apple~CloudDocs/Formazione Aggiuntiva/STATISTICA/DATA/Portfolio Data/Data/"

 
# path to folder that holds multiple .csv files

file_list <- list.files(path=folder, pattern="*.csv") 
# create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the 
#same name as the .csv file

i=1

for (i in 1:length(file_list)){
  
  assign(file_list[i], 
         
         read.csv(paste(folder, file_list[i], sep=''))
         
  )}


Applefull<- merge(`AAPL Historical Data.csv`,`AAPL Historical Data(1).csv`)





matrix<- matrix(c(1:10))
matrix


matrix2<- matrix(c(10:20))
merge.data.frame(matrix,matrix2)

