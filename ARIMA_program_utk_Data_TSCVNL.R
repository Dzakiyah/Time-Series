# ARIMA model for simulation data TSCVNL

library(lmtest)
library(tseries)
library(forecast)


data<-read.table("D:/simulation_data_2020.txt",sep="\t",header=TRUE,dec=",")
dim(data)
y1t <- as.ts(data[,2])

#par(mfrow=c(1,1))
plot(y1t)

par(mfrow=c(1,2))
acf(y1t, lag.max = 48, type = "correlation", ylim=c(-1,1))
pacf(y1t, lag.max = 48, ylim=c(-1,1))

#READ THE DATA FROM TXT#
Ytrain=as.ts(y1t[1:192])               #define training data
Ytest=as.ts(y1t[193:204])              #define testing data

#CHECKING FOR STATIONARY USING ADF TEST#
adf.test(Ytrain)

adf.test(Ytrain, k=12)

#CHECKING FOR STATIONARY USING ACF PLOT#
tick=c(1,12,24,36)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
  #ACF
  acf(Ytrain,lag.max=36,axes=F)
  box()
  axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
  abline(v=tick,lty="dotted", lwd=2, col="grey")
  axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
  #PACF
  pacf(Ytrain,lag.max=36,axes=F)
  box()
  axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
  abline(v=tick,lty="dotted", lwd=2, col="grey")
  axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#DIFFERENCING SEASONAL ORDER FOR YTRAIN
Ytrain_12=diff(Ytrain,lag=12)
par(mfrow=c(1,1))
plot(Ytrain_12)

adf.test(Ytrain_12, k=12)

#ORDER IDENTIFICATION USING ACF AND PACF FROM STATIONARY DATA
tick=c(1,12,24,36)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
  #ACF
  acf(Ytrain_12,lag.max=36,axes=F)
  box()
  axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
  abline(v=tick,lty="dotted", lwd=2, col="grey")
  axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
  #PACF
  pacf(Ytrain_12,lag.max=36,axes=F)
  box()
  axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
  abline(v=tick,lty="dotted", lwd=2, col="grey")
  axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#ARIMA MODELLING#
#Seasonal ARIMA model (not meet white noise assumption)
modelARIMA=arima(Ytrain, order = c(2,0,0),      
                 seasonal = list(order = c(0,1,0),
                                 period =12),
           include.mean=FALSE)
summary(modelARIMA)                         #ARIMA (2,0,0)(0,1,0)12
coeftest(modelARIMA)                        #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)      #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))        #define forecast value for training data

par(mfrow=c(1,1))
acf(resi.ARIMA, lag.max = 48, type = "correlation", ylim=c(-1,1))

#subset ARIMA model (not meet white noise assumption)
modelARIMA=arima(Ytrain, order = c(10,0,0),      
                 seasonal = list(order = c(0,1,0),
                                 period =12),
                 transform.pars = FALSE, 
           fixed=c(0,NA,rep(0,7),NA),                 #NA was the estimated lag, that is : 2,10
           include.mean=FALSE, method = c("ML"))
summary(modelARIMA)                                   #ARIMA ([2,10],0,0)(0,1,0)12
coeftest(modelARIMA)                                  #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                  #define forecast value for training data

par(mfrow=c(1,1))
acf(resi.ARIMA, lag.max = 48, type = "correlation", ylim=c(-1,1))


#subset ARIMA model (meet white noise assumption)
modelARIMA=arima(Ytrain, order = c(35,0,2),      
                 seasonal = list(order = c(0,1,0),
                                 period = 12),
                 transform.pars = FALSE, 
                 fixed=c(0,NA,rep(0,7),NA,0,NA,rep(0,9),NA,NA,
                         rep(0,11),NA,),                 #NA was the estimated lag, that is : 1,2,11,12 and 13
                 include.mean=FALSE, method = c("ML"))
summary(modelARIMA)                                     #ARIMA ([2,10,12,22,23,35],0,0)(0,1,0)12
coeftest(modelARIMA)                                    #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                  #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                    #define forecast value for training data

par(mfrow=c(1,1))
acf(resi.ARIMA, lag.max = 48, type = "correlation", ylim=c(-1,1))

par(mfrow=c(1,1))
plot(Ytrain)
lines(fits.ARIMA, col="red")

fore.ARIMA=predict(fits.ARIMA, 12)$pred        #define forecast value for testing data
se.fore.ARIMA=predict(fits.ARIMA, 12)$se  

#define forecast value for training data

#DIAGNOSTIC CHECKING FOR ARIMA MODEL
#Independency test by using Ljung-Box test
lags <- c(6,12,18,24,30,36,42,48)                     #lag we used
p=3                                                   #the number of ar parameter
q=2                                                   #the number of ma parameter
LB.result<-matrix(0,length(lags),2)
for(i in seq_along(lags))
  {
  LB.test=Box.test (resi.ARIMA, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
  }
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result

#ACF and PACF for RESIDUAL ARIMA MODEL
tick=c(1,12,24,36)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
  #ACF
  acf(resi.ARIMA,lag.max=36,axes=F)
  box()
  axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
  abline(v=tick,lty="dotted", lwd=2, col="grey")
  axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
  #PACF
  pacf(resi.ARIMA,lag.max=36,axes=F)
  box()
  axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
  abline(v=tick,lty="dotted", lwd=2, col="grey")
  axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#Normality test using Kolmogorov Smirnov
ks.test(resi.ARIMA,"pnorm",mean=mean(resi.ARIMA),sd=sd(resi.ARIMA))

#FORECAST FOR TESTING DATA
fore.ARIMA=predict(modelARIMA, 12)$pred        #define forecast value for testing data
se.fore.ARIMA=predict(modelARIMA, 12)$se       #define standard error for forecasting result

#CALCULATE RMSE, MAE, AND MAPE CRITERIA
accuracies=matrix(0,3,2)
colnames(accuracies)=c("Training","Testing")
rownames(accuracies)=c("RMSE","MAE","MAPE")

accuracies[1,1]=accuracy(fits.ARIMA,Ytrain)[1,2]
accuracies[2,1]=accuracy(fits.ARIMA,Ytrain)[1,3]
accuracies[3,1]=accuracy(fits.ARIMA,Ytrain)[1,5]
accuracies[1,2]=accuracy(as.vector(fore.ARIMA),Ytest)[1,2]
accuracies[2,2]=accuracy(as.vector(fore.ARIMA),Ytest)[1,3]
accuracies[3,2]=accuracy(as.vector(fore.ARIMA),Ytest)[1,5]
accuracies

#CONSTRUCT INTERVAL PREDICTION
lower=fore.ARIMA-1.96*se.fore.ARIMA
upper=fore.ARIMA+1.96*se.fore.ARIMA

#COMPARISON BETWEEN ACTUAL AND FORECAST VALUE
a=min(min(fits.ARIMA),min(Ytrain))              #lower bound for training data
b=max(max(fits.ARIMA),max(Ytrain))              #upper bound for training data
c=min(min(fore.ARIMA),min(lower),min(Ytest))    #lower bound for testing data
d=max(max(fore.ARIMA),max(upper),max(Ytest))    #upper bound for testing data

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))  #the number of picture and its margin
par(mgp=c(1.3,0.5,0))                     #the distance between labels and axis

  #PLOTTING FOR TRAINING DATA#
  plot(as.ts(Ytrain),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*0.9,b*1.1))
  box()
  title("Training",line=0.3,cex.main=0.9)
  axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
  axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=seq(1,192,12))
  lines(as.ts(fits.ARIMA),col="red",lwd=2)

  #PLOTTING FOR TESTING DATA#
  plot(as.ts(Ytest),ylab="Yt",xlab="t",lwd=2,ylim=c(a*0.9,b*1.1),cex.lab=0.8,axes=F)
  box()
  title("Testing",line=0.3,cex.main=0.9)
  axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
  axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:12),labels=c(193:204))
  lines(as.vector(fore.ARIMA),col="red",lwd=2)
  lines(as.vector(lower),col="blue2",lty="dotdash",lwd=2)
  lines(as.vector(upper),col="blue2",lty="dotdash",lwd=2)

  #DEFINE THE LEGEND#
  legend("topright",c("Actual","Forecast","Upper Bound","Lower Bound"),
          col=c("black","red","blue2","blue2"),lwd=2,cex=0.7)
  
  