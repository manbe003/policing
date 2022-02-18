#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()

   
#Call in my datasets (UOF)
UOF<-read.csv(file='clean data/Indianapolis/UOF.csv', stringsAsFactors = TRUE)

## Descriptive data ##

#Summary
summary(UOF)

#Standard deviations
sd(UOF$residentAge, na.rm = TRUE)
sd(UOF$officerAge, na.rm = TRUE)
sd(UOF$officerYearsOfService, na.rm = TRUE)

#Variation
var(UOF$residentAge, na.rm = TRUE)
var(UOF$officerAge, na.rm = TRUE)
var(UOF$officerYearsOfService, na.rm = TRUE)

##Graphical analysis
#Outliers
boxplot(UOF$residentAge, ylab = "Resident Age", main = "Boxplot Distribution of Resident Age in UOF")
boxplot(UOF$officerAge, ylab = "Officer Age", main = "Boxplot Distribution of Officer Age in UOF")
boxplot(UOF$officerYearsOfService, ylab = "Officer Years of Service", main = "Boxplot Distribution of Officer Years of Service in UOF")

#Normality test
ggplot(UOF, aes(sample = residentAge)) + geom_qq() + geom_qq_line()
ggplot(UOF, aes(sample = officerAge)) + geom_qq() + geom_qq_line()
ggplot(UOF, aes(sample = officerYearsOfService)) + geom_qq() + geom_qq_line()

#Correlations
cor.test(UOF$officerAge, UOF$residentAge)

## Categorical data analysis ##

#Levels
levels(UOF$residentSex)
levels(UOF$residentRace)
levels(UOF$officerSex)
levels(UOF$officerRace)
levels(UOF$useOfForceReason)
levels(UOF$division)

#Graphs
ggplot(UOF, aes(residentSex)) + 
  geom_bar()
ggplot(UOF, aes(residentRace)) +
  geom_bar()
ggplot(UOF, aes(officerSex)) + 
  geom_bar()
ggplot(UOF, aes(officerRace)) + 
  geom_bar()
ggplot(UOF, aes(useOfForceReason)) + 
  geom_bar()
ggplot(UOF, aes(division)) + 
  geom_bar()

#Comparison
ggplot(UOF, aes(x = residentRace, fill = useOfForceReason)) + 
  geom_bar(position = "dodge")
ggplot(UOF, aes(x = residentRace, fill = officerRace)) + 
  geom_bar(position = "dodge")
ggplot(UOF, aes(x = residentRace, fill = division)) + 
  geom_bar(position = "dodge")
ggplot(UOF, aes(x = residentRace, fill = officerSex)) + 
  geom_bar(position = "dodge")
ggplot(UOF, aes(x = residentRace, fill = residentSex)) + 
  geom_bar(position = "dodge")


