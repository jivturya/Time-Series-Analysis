rm(list=ls())
set.seed(100)
library("urca")

data=read.table("M:/OMSA/ISYE6501/HW3/temps.txt",header=TRUE)
head(data)

#converting data into timeseries class
temp_data=as.vector(unlist(data[,2:21]))
time_series=ts(data=temp_data,start=1996,frequency=123)
plot(decompose(time_series))

#Holtwinters
First_model=HoltWinters(time_series,alpha=NULL,beta=NULL,gamma=NULL,seasonal="additive")
Second_model=HoltWinters(time_series,alpha=NULL,beta=NULL,gamma=NULL,seasonal="multiplicative")
Third_model=HoltWinters(time_series,alpha=NULL,beta=NULL,gamma=FALSE,seasonal="additive")

#SSE Values per model
First_model$SSE
Second_model$SSE
Third_model$SSE

#Choosing Second_Model
Second_model$fitted
plot(Second_model)

#Plotting Second Model COlumns
par(mfrow=c(2,2))
plot(Second_model$fitted[,1])
plot(Second_model$fitted[,2])
plot(Second_model$fitted[,3])
plot(Second_model$fitted[,4])
par(mfrow=c(1,1))

#Extracting reduced noised model data
predicted_values=Second_model$fitted[,1]
predicted_values

#building into a table
Temp_smoothed=matrix(predicted_values,nrow=123)
Temp_smoothed

#output to csv function
write.csv(Temp_smoothed,file="smoothed_function.csv",fileEncoding="UTF-16LE")
