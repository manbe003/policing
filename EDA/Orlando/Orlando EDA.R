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

#deleting rows where number of races and ethnicities dont match up.
UOF_Offenders<- UOF[-c(3041, 4376, 5295, 1125, 3039, 5293, 4374), ]

#separating rows to be one offender per row
UOF_Offenders<- UOF_Offenders %>% separate_rows(Offenders.Race, Offenders.Sex, Offenders.Ethnicity, sep= ";")

#officer data analyses
UOF_officerGroups<- UOF


#separating officer to make white officers with hispanic ethnicity read "hispanic" in the race column
UOF_officerGroups <-separate(UOF_officerGroups, Officers.Race, c('Officer one Race', 'Officer two Race', 'Officer three Race','Officer four Race', 'Officer five Race'), sep=";")
UOF_officerGroups <-separate(UOF_officerGroups, Officers.Ethnicity, c('Officer one ethnicity', 'Officer two ethnicity', 'Officer three ethnicity','Officer four ethnicity', 'Officer five ethnicity'), sep=";")

UOF_officerGroups<- within(UOF_officerGroups, `Officer one Race`[`Officer one Race` == 'White' & `Officer one ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroups<- within(UOF_officerGroups, `Officer two Race`[`Officer two Race` == 'White' & `Officer two ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroups<- within(UOF_officerGroups, `Officer three Race`[`Officer three Race` == 'White' & `Officer three ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroups<- within(UOF_officerGroups, `Officer four Race`[`Officer four Race` == 'White' & `Officer four ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroups<- within(UOF_officerGroups, `Officer five Race`[`Officer five Race` == 'White' & `Officer five ethnicity` == 'Hispanic'] <- "Hispanic" )

#recombine them into one row to make groups
UOF_officerGroups<- unite(UOF_officerGroups,"Officers Race",`Officer one Race`:`Officer two Race`:`Officer three Race`:`Officer four Race`:`Officer five Race`, sep = ";", na.rm = TRUE, remove = FALSE)
UOF_officerGroups<- unite(UOF_officerGroups,"Officers Ethnicity",`Officer one ethnicity`:`Officer two ethnicity`:`Officer three ethnicity`:`Officer four ethnicity`:`Officer five ethnicity`, sep = ";", na.rm = TRUE, remove = FALSE)
UOF_officerGroups<- UOF_officerGroups[-c(7:11,13:17)]
UOF_officerGroups$Officer.Race.Groups <-UOF_officerGroups$`Officers Race`
UOF_officerGroups <-UOF_officerGroups[c(1,2,3,4,5,12,6,7,8,9,10,11)]

#function to make it so mixed race groups say mixed and single race groups say that race in the Officer.Race.Groups col
f <- function(x) ifelse(length(u <- unique(unlist((strsplit(x, ";"))))) > 1, "Mixed", u)

UOF_officerGroups = transform(UOF_officerGroups, Officer.Race.Groups = Vectorize(f)(Officer.Race.Groups))

#summary
summary(UOF)
summary(UOF_Offenders)
summary(UOF_officerGroups)

#levels
levels(UOF_Offenders$Offenders.Race)
levels(UOF_Offenders$Offenders.Ethnicity)
levels(UOF_Offenders$Offenders.Sex)
levels(UOF_officerGroups$Officers.Involved)
levels(UOF_officerGroups$Officer.Race.Groups)


#graphs
ggplot(UOF_Offenders, aes(Offenders.Race)) +
  geom_bar()

ggplot(UOF_Offenders, aes(Offenders.Ethnicity)) +
  geom_bar()

ggplot(UOF_Offenders, aes(Offenders.Sex)) +
  geom_bar()

ggplot(UOF_officerGroups, aes(Officer.Race.Groups)) +
  geom_bar()


#############################

#separating rows so there is only one offender per row
Shootings_Offenders<- Shootings %>% separate_rows(Suspect.Race, Suspect.Gender, Suspect.Hit, Fatal, sep= ",")

#separating officer races and ethnicities so Hispanic officers races say Hispanic
Shootings_OfficerGroups <-Shootings
Shootings_OfficerGroups <-separate(Shootings_OfficerGroups, Officer.Race, c('Officer one Race', 'Officer two Race', 'Officer three Race','Officer four Race', 'Officer five Race', 'Officer six Race', 'Officer seven Race', 'Officer eight Race', 'Officer nine Race', 'Officer ten Race', 'Officer eleven Race'), sep=", |,")
Shootings_OfficerGroups <-separate(Shootings_OfficerGroups, Officer.Ethnicity, c('Officer one ethnicity', 'Officer two ethnicity', 'Officer three ethnicity','Officer four ethnicity', 'Officer five ethnicity', 'Officer six ethnicity', 'Officer seven ethnicity', 'Officer eight ethnicity', 'Officer nine ethnicity', 'Officer ten ethnicity', 'Officer eleven ethnicity'), sep=", |,")

Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer one Race`[`Officer one Race` == 'White' & `Officer one ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer two Race`[`Officer two Race` == 'White' & `Officer two ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer three Race`[`Officer three Race` == 'White' & `Officer three ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer four Race`[`Officer four Race` == 'White' & `Officer four ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer five Race`[`Officer five Race` == 'White' & `Officer five ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer six Race`[`Officer five Race` == 'White' & `Officer six ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer seven Race`[`Officer five Race` == 'White' & `Officer seven ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer eight Race`[`Officer five Race` == 'White' & `Officer eight ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer nine Race`[`Officer five Race` == 'White' & `Officer nine ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer ten Race`[`Officer five Race` == 'White' & `Officer ten ethnicity` == 'Hispanic'] <- "Hispanic" )
Shootings_OfficerGroups<- within(Shootings_OfficerGroups, `Officer eleven Race`[`Officer five Race` == 'White' & `Officer eleven ethnicity` == 'Hispanic'] <- "Hispanic" )

#recombine them into one row to make groups
Shootings_OfficerGroups<- unite(Shootings_OfficerGroups,"Officers Race",`Officer one Race`:`Officer two Race`:`Officer three Race`:`Officer four Race`:`Officer five Race`:`Officer six Race`:`Officer seven Race`:`Officer eight Race`:`Officer nine Race`:`Officer ten Race`:`Officer eleven Race`, sep = ";", na.rm = TRUE, remove = FALSE)
Shootings_OfficerGroups<- unite(Shootings_OfficerGroups,"Officers Ethnicity",`Officer one ethnicity`:`Officer two ethnicity`:`Officer three ethnicity`:`Officer four ethnicity`:`Officer five ethnicity`:`Officer six ethnicity`:`Officer seven ethnicity`:`Officer eight ethnicity`:`Officer nine ethnicity`:`Officer ten ethnicity`:`Officer eleven ethnicity`, sep = ";", na.rm = TRUE, remove = FALSE)
Shootings_OfficerGroups<- Shootings_OfficerGroups[-c(5:15,17:27)]
Shootings_OfficerGroups$Officer.Race.Groups <-Shootings_OfficerGroups$`Officers Race`
Shootings_OfficerGroups <-Shootings_OfficerGroups[c(1,2,3,4,11,5,6,7,8,9,10)]

#function to make it so mixed race groups say mixed and single race groups say that race in the Officer.Race.Groups col
f2 <- function(x) ifelse(length(u <- unique(unlist((strsplit(x, ";"))))) > 1, "Mixed", u)

Shootings_OfficerGroups = transform(Shootings_OfficerGroups, Officer.Race.Groups = Vectorize(f2)(Officer.Race.Groups))

#summaries
summary(Shootings)
summary(Shootings_Offenders)
summary(Shootings_OfficerGroups)

levels(Shootings_Offenders$Suspect.Race)
levels(Shootings_Offenders$Suspect.Gender)
levels(Shootings_Offenders$Suspect.Hit)
levels(Shootings_Offenders$Fatal)
levels(Shootings_OfficerGroups$Officer.Race.Groups)

#graphs
ggplot(Shootings_Offenders, aes(Suspect.Race)) +
  geom_bar()

ggplot(Shootings_Offenders, aes(Suspect.Gender)) +
  geom_bar()

ggplot(Shootings_Offenders, aes(Suspect.Hit)) +
  geom_bar()

ggplot(Shootings_Offenders, aes(Fatal)) +
  geom_bar()

#some comparisons
ggplot(Shootings_Offenders,
       aes(x = Suspect.Race,
           fill = Suspect.Hit))+
  geom_bar(position = "dodge")

ggplot(Shootings_Offenders,
       aes(x = Suspect.Race,
           fill = Fatal))+
  geom_bar(position = "dodge")

