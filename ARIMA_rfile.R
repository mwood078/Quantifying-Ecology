rm(list=ls())

#Load data
load("C:/Users/mwoodsto/Documents/GitHub Repository/Quantifying-Ecology/ARIMA_Workshop.RDATA")

#Load libraries

library(zoo)
library(tseries)
library(forecast)
library(xts)

nee<-ts(mangroves$nee,start=1,frequency=30)

par(mfrow=c(1,1),mai=c(0.25,0.8,0.1,0.1))
plot(nee,typ="l",ylab="NEE",xlab="")

plot(nee)
lines(tsclean(nee),col="red")

nee<-tsclean(nee)
class(nee)

#Time Series Decomposition#
nee.d<-decompose(nee,'multiplicative')
plot(nee.d)


#Stationarity test#
adf.test(nee)

acf(nee,lag.max=45)

pacf(nee,lag.max = 45)


#Fit time series####
arima.nee1<-auto.arima(nee,trace=T)

tsdisplay(residuals(arima.nee1),lag.max=45)

arima.nee2<-arima(nee,order=c(10,1,3),seasonal = list(order=c(2,0,2)))

tsdisplay(residuals(arima.nee2),lag.max=30)

AIC(arima.nee1,arima.nee2)

par(mfrow=c(1,1))
plot(nee,typ="l")
lines(fitted(arima.nee2),col="red")

checkresiduals(arima.nee2,lag=36)

par(mfrow=c(1,1))
plot(nee,typ="l")
lines(fitted(arima.nee2),col="red")

plot(forecast(arima.nee2,h=30))


###Explanatory variables####

#Create TS object#
sal<-ts(mangroves$salinity.max,start=1,frequency=30)

par(mfrow=c(1,1),mai=c(0.25,0.8,0.1,0.1))
plot(sal,typ="l",ylab="Salinity",xlab="")
lines(tsclean(sal),col="red")

sal<-tsclean(sal)

#Decompose#
sal.d<-decompose(sal,'multiplicative')
plot(sal.d)

#Stationarity#
adf.test(sal)

adf.test(diff(sal))

#Correlations#
ccf(diff(sal),nee,na.action = na.pass,lag.max = 40,plot=T)

arima.nee3<-auto.arima(nee,xreg=c(diff(sal),0),trace=T)

AIC(arima.nee3,arima.nee2)

sal.1<-sal
sal.1[sal.1<25]<-0
sal.1[sal.1>=25]<-1
plot(sal.1)

arima.nee4<-auto.arima(nee,xreg=sal.1,trace=T)
AIC(arima.nee2,arima.nee4)
checkresiduals(arima.nee4,lag=36)

par(mfrow=c(1,1))
plot(nee,typ="l")
lines(fitted(arima.nee4),col="red")

#Challenge####
names(mangroves) #Try water.tmax#

#Create TS object#
water_t<-ts(mangroves$water.tmax,start=1,frequency=30)

par(mfrow=c(1,1),mai=c(.5,0.8,0.1,0.1))
plot(water_t,typ="l",ylab="Water Temp",xlab="")
lines(tsclean(water_t),col="red")

water_t<-tsclean(water_t)

#Decompose#
water.t.d<-decompose(water_t,'multiplicative')
plot(water.t.d)

#Stationarity#
adf.test(water_t)

adf.test(diff(water_t))

#Correlations#
ccf(diff(water_t),nee,na.action = na.pass,lag.max = 40,plot=T)

arima.nee5<-auto.arima(nee,xreg=c(diff(water_t),0),trace=T)

AIC(arima.nee5,arima.nee4)

