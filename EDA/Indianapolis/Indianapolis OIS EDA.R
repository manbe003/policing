#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()


#Call in my datasets (OIS)
OIS<-read.csv(file= 'clean data/Indianapolis/OIS.csv', stringsAsFactors = TRUE)

## Descriptive data ##

#Summary
summary(OIS)

#Standard deviations
sd(OIS$residentAge, na.rm = TRUE)
sd(OIS$officerAge, na.rm = TRUE)
sd(OIS$officerYearsOfService, na.rm = TRUE)

#Variation
var(OIS$residentAge, na.rm = TRUE)
var(OIS$officerAge, na.rm = TRUE)
var(OIS$officerYearsOfService, na.rm = TRUE)

##Graphical analysis
#Outliers
boxplot(OIS$residentAge, ylab = "Resident Age", main = "Boxplot Distribution of Resident Age in OIS")
boxplot(OIS$officerAge, ylab = "Officer Age", main = "Boxplot Distribution of Officer Age in OIS")
boxplot(OIS$officerYearsOfService, ylab = "Officer Years of Service", main = "Boxplot Distribution of Officer Years of Service in OIS")

#Normality test
ggplot(OIS, aes(sample = residentAge)) + geom_qq() + geom_qq_line()
ggplot(OIS, aes(sample = officerAge)) + geom_qq() + geom_qq_line()
ggplot(OIS, aes(sample = officerYearsOfService)) + geom_qq() + geom_qq_line()

#Correlations
cor.test(OIS$officerAge, OIS$residentAge)

## Categorical data analysis ##

#Levels
levels(OIS$residentSex)
levels(OIS$residentRace)
levels(OIS$officerSex)
levels(OIS$officerRace)
levels(OIS$division)
levels(OIS$residentWeaponUsed)
levels(OIS$officerWeaponUsed)
levels(OIS$residentCondition)
levels(OIS$officerCondition)

#Graphs
ggplot(OIS, aes(residentSex)) + 
  geom_bar()
ggplot(OIS, aes(residentRace)) + 
  geom_bar()
ggplot(OIS, aes(officerSex)) + 
  geom_bar()
ggplot(OIS, aes(officerRace)) +
  geom_bar()
ggplot(OIS, aes(division)) + 
  geom_bar()
ggplot(OIS, aes(residentWeaponUsed)) +
  geom_bar()
ggplot(OIS, aes(officerWeaponUsed)) +
  geom_bar()
ggplot(OIS, aes(residentCondition)) + 
  geom_bar()
ggplot(OIS, aes(officerCondition)) + 
  geom_bar()

#Comparison
ggplot(OIS, aes(x = residentRace, fill = residentCondition)) +
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = residentRace, fill = officerRace)) +
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = residentRace, fill = division)) + 
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = residentRace, fill = officerSex)) + 
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = residentRace, fill = residentSex)) +
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = officerWeaponUsed, fill = residentRace)) +
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = officerWeaponUsed, fill = officerSex)) + 
  geom_bar(position = "dodge")
ggplot(OIS, aes(x = officerWeaponUsed, fill = officerRace)) + 
  geom_bar(position = "dodge")
