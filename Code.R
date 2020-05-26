
setwd("C:/Users/S.K/Desktop/Insurance_Data_Analysis")


DS <- read.csv("SwedishMotorInsurance.csv")
View(DS)

dim(DS)

#--------------------------------------------

summary(DS)
#--------------------------------------------

cor(DS$Claims,DS$Payment) #--high +ve correlation

cor(DS$Insured,DS$Payment) #--high +ve correlation

plot(DS$Insured,DS$Payment) 

plot(DS$Claims,DS$Payment) 

#----------------------------------------------

lineModel <- lm(Payment ~ ., data = DS)
summary(lineModel)


lineModel <- lm(Payment ~ Claims + Insured + Make + Bonus + Zone + Kilometres, data = DS)
summary(lineModel)

lineModel <- lm(Payment ~ Claims + Insured + Zone + Kilometres, data = DS)
summary(lineModel)

#----------------------------------------------

?apply
ZoneResult <- apply(DS[,c(5,6,7)],2, function(x)tapply(x, DS$Zone, mean))
ZoneResult

KmResult <- apply(DS[,c(5,6,7)],2, function(x)tapply(x, DS$Kilometres, mean))
KmResult

BonusResult <- apply(DS[,c(5,6,7)],2, function(x)tapply(x, DS$Bonus, mean))
BonusResult

#----------------------------------------------------

md <- lm(Claims ~ Kilometres + Zone + Bonus + Make + Insured, data=DS) 

summary(md) 

