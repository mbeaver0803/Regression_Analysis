rm(list=ls())
#data
SP500Quarterly <- read.csv("~/Desktop/Math 441/SP500Quarterly.csv")
HKExchange <- read.csv("~/Desktop/Math 441/HKExchange.csv")
LaborForcePR <- read.csv("~/Desktop/Math 441/LaborForcePR.csv", header=FALSE)

n = length(HKExchange$DATE)

#Making a time series response variable
yt = ts(data = HKExchange$EXHKUS, start = 1, end = n)
#Make a time predictor variable
time = 1:n
#Fit the chosen pth-order autoregressive model
xt = matrix(c(time,time^2),nrow=n)
arfit = arima(x=yt,order=c(1,0,0),xreg=xt)
xtnew = matrix(c(300:302,(300:302)^2), nrow = 3)
ypredict = predict(arfit,n.ahead = 3, newxreg = xtnew)
ypredict
