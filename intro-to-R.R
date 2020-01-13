#file name is intro-to-R

#opening data set
#################
#data available in package
library(MASS) #to ge the package MASS
head(Boston)  #to see the first rows of the data

#data set available as a csv to txt or else
read.csv("MassBodilyInjury.csv", header = TRUE, sep = "," ) #for csv
read.txt("filename.txt", header = TRUE, sep = ",") #for txt

#rename the data set
injury <-read.csv("MassBodilyInjury.csv", header = TRUE, sep = "," )
dim(injury) #tells me rows and columns of data set
head(injury)#tells me first 6 rows

#basic statistics
summary(injury$claims) #$ means to choose a collumn
hist(injury$claims)    #creates histogram of claims
hist(injury$logclaims)




