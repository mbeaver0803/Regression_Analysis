rm(list = ls())
data<-read.csv("~/Desktop/Math 441/AIS.CSV")


#task1

#Create Variables
y<-data$Bfat.
Gender<-as.numeric(data$Gender=="M")
Ht<-data$Ht
Wt<-data$Wt
BMI<-data$BMI
Folds<-data$Folds
BMIsq<-(data$BMI)^2
Htsq<-(data$Ht)^2
Wtsq<-(data$Wt)^2
Foldssq<-(data$Folds)^2
GenderHt<-(Gender*data$Ht)
GenderWt<-(Gender*data$Wt)
GenderBMI<-(Gender*data$BMI)
GenderFolds<-(Gender*data$Folds)
HtFolds<-(data$Ht*data$Folds)
WtFolds<-(data$Wt*data$Folds)
BMIFolds<-(data$BMI*data$Folds)

#create models
ModelA<-lm(y~Gender+Ht+Wt+BMI+Folds+BMIsq+Htsq+Wtsq+Foldssq+
             GenderHt+GenderWt+GenderBMI+GenderFolds+HtFolds+WtFolds+BMIFolds)
summary(ModelA)

ModelB<-lm(y~Gender+BMI+Folds+BMIsq+Foldssq+GenderBMI+GenderFolds+BMIFolds)
summary(ModelB)

ModelC<-lm(y~Gender+Folds+Foldssq+GenderFolds)
summary(ModelC)

#testing two models
anova(ModelA, ModelC)

#testing error assumptions
#mean residual = 0
mean(ModelA$residuals)
mean(ModelB$residuals)
mean(ModelC$residuals)
#normal distribution of error
hist(ModelA$residuals)
hist(ModelB$residuals)
hist(ModelC$residuals)
#constant and small variance
plot(ModelA$residuals)
abline(h=0)
plot(ModelB$residuals)
abline(h=0)
plot(ModelC$residuals)
abline(h=0)
#####################################################

#TASK 2
group1<-subset(data, subset=Gender=="M")
plot(group1$Folds,group1$BMI,main="Folds versus BMI")
linmod<-lm(group1$Folds~group1$BMI)
abline()

summary(ModelB)
summary(ModelC)

RsqraDiff = (ModelB$residuals.sqr)

#####################################################

#TASK 3
#prediction interval
#fitted models
Males<-subset(data, subset=data$Gender=="M")
Females<-subset(data, subset=data$Gender=="F")

yM<-Males$Bfat.
MFolds<-Males$Folds
MFoldssq<-(Males$Folds)^2

ModelCMales<-lm(yM~MFolds+MFoldssq)
ModelCMales2<-lm(yM~MFoldssq)


yF<-Females$Bfat.
FFolds<-Females$Folds
FFoldssq<-(Females$Folds)^2

ModelCFemales<-lm(yF~FFolds+FFoldssq)

summary(ModelCMales)
summary(ModelCFemales)
#males
pat<-data.frame(MFolds+MFoldssq,yM)

t.test(pat)

pat = data.frame(MFolds = 70, MFoldssq=4900)
predict(ModelCMales, pat, interval = "confidence")
#females
pat<-data.frame(FFolds+FFoldssq,yF)

t.test(pat)

pat = data.frame(FFolds = 70, FFoldssq=4900)
predict(ModelCFemales, pat, interval = "confidence")
#####################################################







