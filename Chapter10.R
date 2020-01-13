#time series

#white noise
rm(list = ls())
w=rnorm(500,0,1)
par(mfrow=c(1,1))
plot(w)
ts.plot(w,main = "white noise")

#moving averages MA(q)
v=filter(w, sides=2,rep(1/3,3))
ts.plot(v, ylim=c(-3,3), main="moving average")

#Autoregression AR(p)
#x(t) = 0x(t-1) -0.9x(t-2)
#option 1: using filter

x=filter(w,filter = c(1,-.9), method = "recursive")
ts.plot(x, main="autoregression")


#problem
  #white noise
n=rnorm(100,0,1)
ts.plot(n,main="white noise")
  #moving averages
q = filter(n, sides = 2, rep(1/4,4))
ts.plot(q,ylim=c(-3,3), main = "moving average")
  #autoregression
r=filter(n,filter = c(1,-.9), method = "recursive")
ts.plot(r, main = "Autoregression")
