#libraries
library(tidyr)
library(dplyr)
library(here)
library(tidyverse)
library(stringr)

##Categorical data because there is no descriptive data
#call files
Shootings<-read.csv(file=here('clean data/orlando/Shooting (cleaned).csv'), stringsAsFactors = FALSE)
UOF<-read.csv(file=here('clean data/orlando/UOF (cleaned).csv'), stringsAsFactors = FALSE)

#deleting rows where there are 6 races listed, but only 5 ethnicities listed.
UOF<- UOF[-c(3041, 4376, 5295, 1125, 3039, 5293, 4374), ]

UOF<- UOF %>% separate_rows(Offenders.Race, Offenders.Sex, Offenders.Ethnicity, sep= ";")

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


#officer data analyses
UOF_officerGroups<- UOF
UOF_officerGroups$Officer.Race.Groups <-UOF_officerGroups$Officers.Race
UOF_officerGroups <-UOF_officerGroups[c(1,2,3,4,5,12,6,7,8,9,10,11)]


#making officers into groups
UOF_officerGroups$Officer.Race.Groups <-gsub('Asian;Asian', 'Asian', UOF_officerGroups$Officer.Race.Groups)
UOF_officerGroups$Officer.Race.Groups <-gsub('Black;Black|Black;Black;Black|Black;Black;Black;Black','Black', UOF_officerGroups$Officer.Race.Groups)
UOF_officerGroups$Officer.Race.Groups <-gsub('White;White|White;White;White|White;White;White;White|White;White;White;White;White|White;White;White;White;White;White|White;White;White;White;White;White;White;White', 'White', UOF_officerGroups$Officer.Race.Groups)
UOF_officerGroups$Officer.Race.Groups <-gsub('Black;Asian;White;White|White;Black;White;White;Black;White;White;White|White;Black;White;White;White;White;Black;White|Black;Asian;White|
                                              White;White;Black;White;Black;White|White;Asian;White;Asian;White|White;Asian;White;Black;White|White;Black;Asian;Black|
                                              White;Asian;White;White;White;White;Black|Asian;Black|White;Black|White;Black;Black|Asian;White|Black;White|White;White;Black|
                                              White;White;Black;White;White;White|Black;White;White;White;Black| Black;Black;Black;White|White;White;Black;White|White;Black;White;White;White|
                                              White;Black;White|White;Asian|Black;White;White;White|White;Asian;White;Black;White|White;White;White;White;White;White;White;Asian|
                                              White;White;White;Black|Black;White;White|Black;White;Black|White;White;Black;White;Black;White|Asian;White;White|                              
                                              Black;Asian|White;Asian;White|Black;White;Black;White|White;Black;White;White|
                                              White;White;White;Asian;White;White|White;White;White;Black;Black|White;White;White;White;Black;White;White|Black;White;White;White;White;White|            
                                              White;White;White;Black;White|White;White;White;White;White;White;Black;Black|White;White;White;White;Black;White|Black;White;White;Black|
                                              Black;Black;White|White;White;White;Asian|White;White;Asian;White|Black;Black;White;Black|White;White;Black;White;White;Black;White|
                                              Asian;Black|White;Black;White;Asian|Asian;White;White;White|White;White;Asian|Asian;White;Black|White;Black;Black;Black|Black;White;Asian|                              
                                              Black;Asian;White|White;Black;White;White;White;Black|White;Asian;White;White;White;White;Black|White;Asian;White;White|White;Black;Asian|                               
                                              White;White;White;White;White;Black|Black;White;White;White;White;White;Black;White|White;Black;White;White;White;White;White|Asian;Black;Black|                        
                                              Asian;Black;White;White|White;White;Black;White;White|White;Black;White;Black|Black;Black;White;White;Black;Black;White|Asian;White;White;White;White;White|          
                                              White;White;Asian;White;White;White|Black;Black;White;White|Black;White;White;White;White|White;Asian;White;White;White;White;White|Black;Asian;Black|                              
                                              Asian;Asian;White|White;Black;White;White;White;White;Black;White|White;White;White;White;Black|White;White;Asian;White;White|White;White;Black;Black|
                                              White;Asian;White;Asian;White|White;White;Asian;Black|White;White;Black;White;Black|White;White;Black;Black;White;White|Black;White;White;Black;White|                   
                                              White;Black;White;White;Black;White;White;White|White;Asian;White;White;White;White|Asian;Black;White|White;Black;Asian;White|White;White;White;Black;White;White|           
                                              White;Black;Asian;Black|White;Asian;Asian|Asian;Black;White;Black|White;White;White;White;Asian|White;White;White;Asian;White|White;Black;Black;White|
                                              White;Black;White;White;White;White|Asian;White;White;Black|White;White;Black;Asian|Black;White;Black;White;White|White;Asian;Black|White;Asian;White;White;White;White;White;White|
                                              White;White;White;White;Black;Black|White;White;Black;White;White;White;White;White|Black;Asian;White;White', 'mixed', UOF_officerGroups$Officer.Race.Groups)


ggplot(UOF_officerGroups, aes(Officer.Race.Groups)) +
  geom_bar()



#############################
Shootings<- Shootings %>% separate_rows(as.character(Suspect.Race), sep= ",")


