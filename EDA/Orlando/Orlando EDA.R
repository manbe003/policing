#libraries
library(tidyr)
library(dplyr)
library(here)
library(tidyverse)

##Categorical data because there is no descriptive data
#call files
Shootings<-read.csv(file=here('clean data/orlando/Shooting (cleaned).csv'), stringsAsFactors = FALSE)
UOF<-read.csv(file=here('clean data/orlando/UOF (cleaned).csv'), stringsAsFactors = FALSE)

#deleting rows where there are 6 races listed, but only 5 ethnicities listed.
UOF<- UOF[-c(3041, 4376, 5295, 1125), ]

UOF<- UOF %>% separate_rows(Offenders.Race, Offenders.Sex, Offenders.Ethnicity, sep= ";")

UOF_Officers<- UOF[-c(12,6,7,5,13,14,1,2,3,8,9,10,15,16,17,4,11,20,25,26,27,28,29,40,41,42,21,22,23,50,37,1936,45,49,24,37,32,33,34,35,51,52,48,46,31,38,30,18,43,44,53,39), ]
UOF_tryin<- UOF %>% separate_rows(Officers.Race, Officers.Ethnicity, Officers.Sex, sep = ";")

summary(UOF)
summary(Shootings)

#levels
levels(UOF$Officers.Race)
levels(UOF$Officers.Ethnicity)
levels(UOF$Officers.Sex)
levels(UOF$Offenders.Race)
levels(UOF$Offenders.Ethnicity)
levels(UOF$Offenders.Sex)

#graphs
ggplot(UOF, aes(Offenders.Race)) +
  geom_bar()

ggplot(UOF, aes(Offenders.Ethnicity)) +
  geom_bar()

ggplot(UOF, aes(Offenders.Sex)) +
  geom_bar()



#############################
Shootings<- Shootings %>% separate_rows(as.character(Suspect.Race), sep= ",")


