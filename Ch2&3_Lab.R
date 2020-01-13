rm(list = ls())
Patients = read.table("FACTORS.TXT", header = TRUE)
attach(Patients)

summary(Patients)
plot(Patients$FACTORS, Patients$LOS)

x<-Patients$FACTORS
y<-Patients$LOS

lm(y~x)
linMod<- lm(y~x, data = Patients)
mean(linMod$residuals)
plot(linMod$residuals)

anova(linMod)
s = sqrt(4.413)
cv = (s/mean(y))*100
cv

pat = data.frame(x,y)

t.test(pat)

pat = data.frame(x = 231)

predict(linMod, pat, interval = "confidence")
