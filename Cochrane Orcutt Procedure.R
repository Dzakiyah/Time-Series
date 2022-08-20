library(car)
library(blaise)
library(lmtest)

data=read.csv("D:/PraktikumAD_Week10.csv", header=T, sep=";")
head(data)

#Regression OLS
model1=lm(Y~X, data=data)
summary(model1)

#Durbin-watson
dwt(model1)

#Cochrane Orcutt Procedure
#Create Lag residuals
res.ts=residuals(model1)[2:30]
lag1res=residuals(model1)[1:29]

#Bind time series data
lagdata1=data.frame(res.ts, lag1res)

#Regreesion OLS to get the rho coef (respon=residual, prediktor=lag residual)
acp=coef(lm(res.ts~lag1res -1, data=lagdata1))
acp

#Cochrane Orcut Transformation
#lag for x and y
lag1y=data$Y[1:29]
y=data$Y[2:30]
lag1x=data$X[1:29]
x=data$X[2:30]

#X-(rho*lagX)
y.co=y-(acp*lag1y)
x.co=x-(acp*lag1x)

#Regression OLS using transformation data
model2=lm(y.co~x.co)
summary(model2)
dwtest(model2)