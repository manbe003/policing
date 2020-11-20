#set WD
setwd("C:/Users/katie/Desktop/policing/clean data/Orlando")

#load library
library(tidyverse)

##Categrical data because there is no descriptive data
#call files
Shootings<-read.csv("Shooting (cleaned).csv", stringsAsFactors = TRUE)
UOF<-read.csv("UOF (cleaned).csv", stringsAsFactors = FALSE)

summary(UOF)
summary(Shootings)

#levels
levels(UOF$Officers.Race)
levels(UOF$Officers.Ethnicity)
levels(UOF$Officers.Sex)
levels(UOF$Offenders.Race)
levels(UOF$Offenders.Ethnicity)
levels(UOF$Offenders.Ethnicity)
levels(UOF$Offenders.Sex)

#graphs
ggplot(UOF, aes(Officers.Race)) +
  geom_bar()
