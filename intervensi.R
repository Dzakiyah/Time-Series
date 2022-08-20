#################################################
library(vars)
library(tseries)
library(astsa)
library(forecast)
library(zoo)


#import data and prepare data
library(readxl)
data <- read_excel("D:/sahamIHSG.xlsx")
data = cbind(data$Date, data$Close, data$St)
colnames(data)=c("Date","Close","St")

ihsg=data[1:601,2]
plot.ts(ihsg)
#Kasus covid pertama 2 maret 2020
abline(v=262, col="red", lty="dashed", lwd=2)

#potong data
ihsg.pre=ihsg[1:262]
plot.ts(ihsg.pre)

#1. Identifikasi model ARIMA sebelum intervensi (T=262)
#model step

#cek white noise
Box.test(ihsg.pre,lag=12, fitdf = 0,type="Lj") #terjadi white noise, tedapat model yang sesuai

#cek plot ACF dan PACF
acf2(ihsg.pre, max.lag=24)
#:Data belum stasioner karena plot ACF masih tails off
acf2(diff(ihsg.pre), max.lag = 24)

#cek model ARIMA yang sesuai
model1 <- auto.arima(ihsg.pre, max.d=1, max.D=1, stepwise=FALSE, trace=TRUE)#diperoleh model ARIMA(0,1,1)

checkresiduals(model1)
Box.test(model1$residuals, lag = 24, type = "Ljung-Box")

coeftest(model1)

#Lakukan forecasting
fc <- forecast(model1, h=210)
# Combine with observed data
gab <-ts.union(ts(ihsg[263:472]),ts(as.numeric(fc$mean)))
colnames(gab)=c("Actual","Forecasting")
#  cbind(ihsg[156:175],as.numeric(fc$mean))

# Plot
plot(gab, type="l", plot.type="s", col=c('blue','red'), linetype=c("solid","dashed"))

#cek selisih forecasting
selisih=gab[,2]-gab[,1]
plot(selisih[1:10],type="h")
#dugaan b=1, r=2, s=1

library(TSA)
#Model interfensi, lakukan coba2 terhadap kemungkinan r dan s yang bisa terjadi, cek AIC terkecil dan cek asumsi residual
model.in <- arimax(ihsg.pre, order=c(0,1,1), xtransf=model1$residuals, method = 'ML', transfer=list(c(8,0,1)), include.mean = FALSE)
coeftest(model.in)

#Cek asumsi residual
checkresiduals(model.in)
Box.test(model.in$residuals, lag = 24, type = "Ljung-Box")

#Forecasting
fore <- as.ts(fitted(model.in))

lines(fc, col="red")
plot.ts(ihsg + lines(fc, col="red"))

gb <-ts.union(ts(ihsg[263:472]),ts(as.numeric(fore)))
colnames(gb)=c("Actual","Forecasting")
cbind(ihsg[263:472], fore)
# Plot
plot(gb, type="l", plot.type="s", col=c('blue','red'), linetype=c("solid","dashed"))
