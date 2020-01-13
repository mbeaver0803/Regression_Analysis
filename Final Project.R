#chp10-time series full example 1
#sales prices example
#data: sales 35
#library(lmtest) needed for Durbin Watson test

#Introductory descriptive statistics
rm(list = ls())
Asales<-read.csv("AlcoholSales.csv", header = TRUE)
attach(Asales)
summary(Asales)
sd(sales)
hist(sales, breaks = 10)

#variables
y<-Asales$Sales
Lterm<-Asales$Time
Qterm<-Lterm^2
Sterm1<-as.numeric(Asales$Month=="January")
Sterm2<-as.numeric(Asales$Month=="February")
Sterm3<-as.numeric(Asales$Month=="March")
Sterm4<-as.numeric(Asales$Month=="April")
Sterm5<-as.numeric(Asales$Month=="May")
Sterm6<-as.numeric(Asales$Month=="June")
Sterm7<-as.numeric(Asales$Month=="July")
Sterm8<-as.numeric(Asales$Month=="August")
Sterm9<-as.numeric(Asales$Month=="September")
Sterm10<-as.numeric(Asales$Month=="October")
Sterm11<-as.numeric(Asales$Month=="November")
Sterm12<-as.numeric(Asales$Month=="December")


#t=1:dim(sales35)[1]
plot(Lterm, y)
ts.plot(y, main = "Sales v. Time")

#+Sterm1+Sterm2+Sterm3+Sterm4+Sterm5+Sterm6+Sterm7+Sterm8+Sterm9+Sterm10+Sterm11

#simple regression
model1<-lm(y~Lterm+Sterm10+Ster11+Sterm12)
summary(model1)
model2<-lm(y~Lterm+Qterm+Sterm12)
summary(model2)

#Anova test
anova(model1,model2)

#prediction 95% prediction interval for years 36 to 40
timpred<-data.frame(Lterm=287, Qterm=287^2, Sterm=1)
predict(model2,timpred,level = 0.95, interval = "prediction")

#Autoregression plot
#time series

#white noise
plot(y)
ts.plot(y,main = "white noise")

#moving averages MA(q)
v=filter(y, sides=2,rep(1/3,3))
ts.plot(v, ylim=c(1000,5000), main="moving average")

#Autoregression AR(p)
#x(t) = 0x(t-1) -0.9x(t-2)
#option 1: using filter

x=filter(y,filter = c(1,-.9), method = "recursive")
ts.plot(x, main="autoregression")


#problem
#white noise
ts.plot(y,main="white noise")
#moving averages
q = filter(y, sides = 2, rep(1/4,4))
ts.plot(q,ylim=c(1000,5000), main = "moving average")
#autoregression
r=filter(y,filter = c(1,-.9), method = "recursive")
ts.plot(r, main = "Autoregression")


#prediction
n = length(y)
yt = ts(data = y, start = 1, end = n)
xt = matrix(c(Lterm,Qterm), nrow = n)
arfit = arima(x=yt,order=c(0,0,1),seasonal = list(order=c(0,0,1)), xreg=xt)
xtnew = matrix(c(1:5, (1:5)^2), nrow = 5)
ypredict = predict(arfit, n.ahead = 3, newxreg = xtnew)
ypredict


#task 2
#testing of residuals
mean(model2$residuals)
plot(model2$residuals)
abline(h=0) 
qqnorm(model2$residuals)
qqline(model2$residuals)

#Durbin-Watson Test to check autocorrelation of residuals
#needs package lmtest
#range between 0 and 4
#values less than 2 indicates positively correlated residual
#values larger than 2 indicated neg correlated
library(zoo)
library(lmtest)
(dwtest(model2))

#residual analysis
(plot(time,model2$residuals))

#modeling the residuals as ARIMA
#for example AR(1) is the ARIMA(1,0,0)
residmodel<-data.frame(model2$residuals)
#ts=time series    start=starting time
residmodeltimeser<-ts(data =residmodel, start = 1, end = 297)

#modeling the residuals as ARIMA
#For example AR(1)  is ARIMA(1,0,0)
arresidtimeser<-arima(residmodeltimeser,order = c(0,0,1))
(mean(arresidtimeser$residuals))

#analysis of residuals from ARIMA
hist(arresidtimeser$residuals, breaks = 8)
qqnorm(arresidtimeser$residuals)
qqline(arresidtimeser$residuals)

##October 2016
error1 = model2$residuals
error2 = arresidtimeser$residuals
model3<-lm(y~Lterm+Qterm+Sterm10+Sterm11+Sterm12+error2)
timpred<-data.frame(Lterm=294:297,Qterm=(294:297)^2,Sterm10=0,Sterm11=0,Sterm12=0, error2 = mean(arresidtimeser$residuals))
predict(model3,timpred)

