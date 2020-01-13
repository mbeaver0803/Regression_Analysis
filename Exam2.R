rm(list = ls())
IPO = read.csv("IPO2.CSV", header = TRUE)
attach(IPO)
names(IPO)

summary(IPO$REV)
summary(IPO$PRICEIPO)
hist(IPO$REV)
hist(IPO$PRICEIPO)
#Log
x = log(IPO$REV)
y = log(IPO$PRICEIPO)

summary(x)
summary(y)
hist(x)
hist(y)



plot(IPO$REV, IPO$PRICEIPO, main = "Scatterplot of Revenue and IPO Price", xlab = "Revenue", ylab = "IPO Price")
plot(x,y, main = "Scatterplot of Revenue and IPO Price", xlab = "Revenue", ylab = "IPO Price")

Rev<- IPO$REV
Price <- IPO$PRICEIPO

lm(y~x)
plot(lm(Price~Rev))
plot(lm(y~x))

linMod <- lm(y~x, data = IPO)

plot(linMod$residuals, main = "Scatterplot of Residuals", xlab = "Revenue", ylab = "IPO Price")

mean(linMod$residuals)
mean(linMod2$residuals)

#Variance of residuals
anova(linMod1)
anova(linMod2)
s = sqrt(14377432)
mean(Price)
mean(y)
cv = (s/mean(Price))*100
cv
cv = (s/mean(y))*100
#Distribution
hist(linMod$residuals)
boxplot(linMod$residuals)
qqnorm(linMod$residuals)
qqline(linMod$residuals)

#check utility of hypothesized 
x = IPO$REV
y = IPO$PRICEIPO
lott = data.frame(x,y)
t.test(lott)

predict(linMod, pat, intervale = "confidence", level = .95)