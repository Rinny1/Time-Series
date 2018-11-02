

1#----------------------------------Setting Up Working Directory----------------------------#
Path<-"C:\Users\raveesh\Desktop\IVY\R\Time Series"
setwd(Path)
getwd()

data<-read.csv("1sales (1).csv",header = TRUE)
TSdata=data


2#---------------------------------Exploring the data-------------------------------------------------------#
head(TSdata)
dim(TSdata)
str(TSdata)
summary(TSdata)
str(TSdata)
colSums(is.na(TSdata))
names(TSdata)[c(1:2)]=c("Date","Sales")
TSdata

3#---------------------Transformation of the date data into time series------------------------------------#

TSdata=ts(TSdata[,2],start=c(2003,1),frequency=12)
start(TSdata)
end(TSdata)
frequency(TSdata)

4#---------------------------Plotting The sales------------------------------------------#
plot(TSdata,ylab="Sales", xlab="Year",main="Sales between 2003-2017",col="blue")
abline(reg = lm(TSdata~time(TSdata)))

5#---------------------------Log Transformation of Data-----------------------------------
plot(log10(TSdata),ylab="Log on Sales",xlab="Year",main="Log(Sales) between 2003-2017",col="grey")
data1<-log10(TSdata)

adf.test(data1,alternative="stationary")
#since p=0.9691>0.05,which shows that series is not stationary

#---------------------------Differencing The Data-----------------------------------

1.#--------------->1st order of Differencing the data

plot(diff(TSdata),ylab="1st order difference Sales",xlab="Year",main="Diff(Sales) between 2003-2017",col="grey")
data2<-diff(TSdata)
adf.test(data2,alternative = "stationary")

#since p=0.08252>0.05,which shows that series is not stationary


#--------------->Differencing+Log transformation------------------------------------# 

plot(diff(log10(TSdata),differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales)) between 2003-2017",col="grey")
LDTSdata=diff(log10(TSdata))
require(forecast)
adf.test(LDTSdata,alternative="stationary")

#since p=0.206>0.05,which shows that series is not stationary

plot(diff(log10(TSdata),differences = 2),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales)) between 2003-2017",col="grey")
LDTSdata1=diff(log10(TSdata),differences = 2)
require(forecast)
adf.test(LDTSdata1,alternative="stationary")

#So, with Log and 2 order of differencing makes the series stationary



#----------------------Creating the ACF and PACF plot------------------------------------#
par(mfrow=c(1,2))
acf(diff(log10(TSdata),main="ACF plot"))
pacf(diff(log10(TSdata),main="PACF plot"))


#------------------------Running the ARIMA model----------------------------------------#
ARIMAFit=arima((log10(TSdata)),c(0,2,0))
summary(ARIMAFit)

#---------------------------Running the ARIMA model-R, gives the best model fit--------------# 
require(forecast)
ARIMAFit1=auto.arima(log10(TSdata),approximation=TRUE,trace=TRUE)
summary(ARIMAFit1)

#-------------------------Predicting the future values----------------------------------------#
pred=predict(ARIMAFit1,n.ahead=36)
pred

##Ploting the observed data and forecasted data together
par(mfrow=c(1,1))
plot(TSdata,type="l",xlim=c(2004,2020),ylim=c(1,110430),xlab="Year",ylab="Sales")
lines(10^(pred$pred),col="red")

#plotting the +-2 standard error to rnage of expected error
plot(TSdata,type="l",xlim=c(2004,2020),ylim=c(1,110430),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue")
lines(10^(pred$pred-2*pred$se),col="orange")

## then forecast the result
pred = predict(ARIMAFit1, n.ahead = 36)
write.csv(pred,"predict.csv")

## then do the exponential since we used log earlier.
normal_result=10^pred$pred
normal_result
write.csv(normal_result,"finalpredict.csv")
plot(normal_result)

#-------------------------------End of the model-----------------------------------#


