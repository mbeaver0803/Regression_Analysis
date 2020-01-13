#Ch 3
rm(list = ls())
Lottery = read.csv("WiscLottery.csv", header = TRUE)
attach(Lottery)
names(Lottery)

#Step 2

summary(Lottery$POP)
summary(Lottery$SALES)
hist(Lottery$POP)
hist(Lottery$SALES)

dim(Lottery$POP)
dim(Lottery) #gives (row = n) and collumns 

plot(Lottery$POP, Lottery$SALES)

pop<- Lottery$POP
sales<- Lottery$SALES

lm(sales~pop)
plot(lm(sales~pop))

linMod <- lm(sales~pop, data = Lottery)
 
plot(linMod)

mean(linMod$residuals)

#Variance of residuals
anova(linMod)
s = sqrt(14377432)
mean(sales)
cv = (s/mean(sales))*100
cv
#Distribution
hist(linMod$residuals)
boxplot(linMod$residuals)
qqnorm(linMod$residuals)
qqline(linMod$residuals)

#check utility of hypothesized 
x = Lottery$POP
y = Lottery$SALES
lott = data.frame(x,y)
t.test(lott)

predict(linMod, pat, intervale = "confidence", level = .95)

