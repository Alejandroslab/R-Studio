#If you don't have install firs install the package
#install.packages('neuralnet')

#Then load it
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput) #inserisce i dati casuali e la loro radice in una matrice
colnames(trainingdata) <- c("Input","Output") #da il nome alle colonne

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.

net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.02, rep = 1, stepmax = 20, linear.output = F)

print(net.sqrt)

#Plot the neural network
plot(net.sqrt)





#Test the neural network on some training data
testdata <- as.data.frame((1:20)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Exp.Output","Neur.Output")
print(cleanoutput)
