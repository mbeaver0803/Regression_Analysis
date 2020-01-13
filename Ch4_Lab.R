#Lab Chapter 4 & 5
#Part1, Chapter 4
rm(list = ls())
Geese <- read.table(file = "SNOWGEESE.txt", header = TRUE)
#variables
y = Geese$WtChange
x1 = Geese$DigEff
x2 = Geese$ADFiber

#Linear Model
lm1 <- lm(y~ x1)
lm2 <- lm(y~x2)
lm3 <- lm(y~x1+x2)
lm4 <- lm(y~x1+x2+(x1*x2))
lm5 <- lm(y~x2+x2+(x1*x2)+(x1^2)+(x2^2))

#F-Test
summary(lm1)
summary(lm5)

RsqrADiff = (summary(lm1)$adj.r.squared)-(summary(lm5)$adj.r.squared)

#Part 2, chapter 5
TurbineHeat <- read.table(file = "GASTURBINE.txt", header = TRUE)
y = TurbineHeat$HEATRATE
rpm = TurbineHeat$RPM
cpr = TurbineHeat$CPRATIO
#linear models
lm4 <- lm(y~rpm+cpr+(rpm*cpr))
lm5 <- lm(y~rpm+cpr+(rpm*cpr)+(rpm^2)+(cpr^2))

#F-Test
summary(lm4)
summary(lm5)

RsqrADiff = (summary(lm5)$adj.r.squared)-(summary(lm4)$adj.r.squared)

anova(lm5,lm4)
