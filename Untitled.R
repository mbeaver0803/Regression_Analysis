

AutoBI <- read.csv("~/Desktop/Math 441/AutoBI.csv")

y<-AutoBI$LOSS
x1<-AutoBI$ATTORNEY
x2<-AutoBI$CLMSEX
x3<-AutoBI$MARITAL
x4<-AutoBI$CLMINSUR
x5<-AutoBI$CLMAGE
x6<-AutoBI$SEATBELT

#create models
model1<-lm(y~x1+x2+x3+x4+x5+x6, data = AutoBI)
model1
summary(model1)
anova(model1,model2)
plot(model1$fitted.values, model1$residuals)
abline(h=0)

model2<-lm(y~x2+x3+x4+x5+x6, data = AutoBI)
model2
summary(model2)

#goups
group1<-model1
y1<-group1$LEASEFEE
x1<-group1$SIZE
x1sq<-x1^2
group1["y1"]<-y1
group1["x1"]<-x1
View(group1)

group2<-subset(HAWAII, subset = x>12)
y2<-group2$LEASEFEE
x2<-group2$SIZE
x2sq<-x2^2
group2["y2"]<-y2
group2["x2"]<-x2
View(group2)

#group models and analysis
model11<-lm(y1~x1+x1sq, data = HAWAII)
summary(model11)
anova(model11)

model12<-lm(y2~x2+x2sq, data = HAWAII)
summary(model12)
anova(model12)

#transformation

#mulitplicative
ystar1<-log(y)
HAWAII["ystar1"]<-ystar1
model2<-lm(ystar1~x+xsq, data = HAWAII)
plot(model2$fitted.values, model2$residuals)
abline(h=0)
#Binomial
ystar2=asin(sqrt(y))
HAWAII["ystar2"]<-ystar2
(model3<-lm(ystar2~x, data= HAWAII))
plot(model3$fitted.values, model3$residuals)

#Hypothesis test for variance by splitting two groups
#Statistic
#F = MSE(df1 = n1-num of param)/MSE(df2 = n2- num of param)
#Pvalue = F stat







