#Fed 15;
#problems on Ch. 3;
rm(list = ls())
#to plot several lines on the same plot
curve(3+2*x, xlim = c(-5,5), ylim = c(-5,5))
abline(1,1,col = 'red')
#ch3 problem section 3.3
EX36<-read.table(file = "EX3_6.txt", header = TRUE)
fit<-lm(y~x, data = EX36)
Y<-EX36$Y
X<-EX36$X

summary(lm(Y~X, data = EX36))

#chp3 problem 3-18
#SSE
SSE = sum((y-fit$fitted.values)^2)
SSE

#chp3 problem 3-4
a = curve(3+2*x, xlim = c(-5,5), ylim = c(-5,5), col = 'blue')
b = abline(1,1,col = 'green')
d = abline(5,0,col = 'red')
c = abline(-2,3,col = 'purple')
e = abline(4,-2,col = 'orange')

#ch3 problem 3-7
x = c(-2:2)
y = c(4,3,3,1,-1)

EX37 = data.frame(x,y)
plot(EX37)
lm(y~x)
abline(lm(x~y))
EX37 = lm(y~x)
summary(EX37)
mean(EX37$residuals)






