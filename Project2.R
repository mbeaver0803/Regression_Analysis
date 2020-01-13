rm(list = ls())
#Task 1
boysgrowth=read.csv("BOYSGROWTHDATA.CSV", header = TRUE)
allgrowth=read.csv("ALLGROWTHDATA.CSV", header = TRUE)


library(ggplot2)
attach(allgrowth)
gplot1<- ggplot(data=allgrowth, aes(x = allgrowth$ht2, y = allgrowth$ht18))+geom_point(aes(colour = factor(gender)))
gplot1
#Task 2
lm(boysgrowth$ht2 ~ boysgrowth$ht18)
lm.r=lm(boysgrowth$ht2 ~ boysgrowth$ht18)
summary(lm.r)

#Task 3
lm(boysgrowth$ht9 ~ boysgrowth$ht18)
lm.r=lm(boysgrowth$ht9 ~ boysgrowth$ht18)
summary(lm.r)

#Task 4
x<-boysgrowth$ht2
y<-boysgrowth$ht18
newdata<-data.frame(x=92)
reg1<-lm(y ~ x)
predict(reg1, newdata, interval = 'confidence')
##Predict the height for an 18 yr old
predict(reg1,newdata, interval = 'predict')

##Estimate mean height for the 18 yr old boys with height 144 cm at age 9
x<-boysgrowth$ht9
y<-boysgrowth$ht18
newdata<-data.frame(x=144)
reg1<-lm(y ~ x)
predict(reg1, newdata, interval = 'confidence')
##Predict the height for an 18 yr old
predict(reg1,newdata, interval = 'predict')

