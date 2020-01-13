#file name intro-2
#install.packages("ggplot2") to install packages
library(ggplot2) #to open package after installation
#to have a look
head(mpg)#or
head(mpg, n=10)
dim(mpg)
#to get the variables
?mpg
#to select column
mpg$year #or two columns

#to get historgram:
#hist(mpg$cty)
hist(mpg$cty, breaks = 12, col = "blue")

#to compare several boxplots;
#independant variable "drv"
#f "front-wheel drive" r"read-wheel drive" 4"4wd"
#repsonse variable hmp"highway miles per gallon"

boxplot(hwy~drv, data = mpg)

#ANOVA
aov(hw~drv, data = mpg)
summary (aov(hwy~drv,data = mpg))

#example two

pain = c(4,5,4,3,2,4,3,4,4,6,8,4,5,4,6,5,8,6,6,7,6,6,7,5,6,5,5)
drug = c(rep("A",9), rep("B", 9), rep("C", 7))
migraine = data.frame(pain,drug)

boxplot(pain~drug,data = migraine)
aov(pain~drug,data = migraine)
summary(aov(pain~drug,dtat = migraine))
TukeyHSD(aov(pain~drug, data = migraine))

#option with the ggplot
ggplot(data = mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + theme_minimal() + 
  ylab("Highway Miles Per Gallon") + 
  xlab("Type of Drive") 
