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
UOF_Offenders<- UOF[-c(3041, 4376, 5295, 1125, 3039, 5293, 4374), ]

UOF_Offenders<- UOF_Offenders %>% separate_rows(Offenders.Race, Offenders.Sex, Offenders.Ethnicity, sep= ";")

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
#making a second dataframe for another way of doing this
UOF_officerGroupsTest <-UOF_officerGroups

#separating officer to make white officers with hispanic ethnicity read "hispanic" in the race column
UOF_officerGroupsTest <-separate(UOF_officerGroupsTest, Officers.Race, c('Officer one Race', 'Officer two Race', 'Officer three Race','Officer four Race', 'Officer five Race'), sep=";")
UOF_officerGroupsTest <-separate(UOF_officerGroupsTest, Officers.Ethnicity, c('Officer one ethnicity', 'Officer two ethnicity', 'Officer three ethnicity','Officer four ethnicity', 'Officer five ethnicity'), sep=";")

UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer one Race`[`Officer one Race` == 'White' & `Officer one ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer two Race`[`Officer two Race` == 'White' & `Officer two ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer three Race`[`Officer three Race` == 'White' & `Officer three ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer four Race`[`Officer four Race` == 'White' & `Officer four ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer five Race`[`Officer five Race` == 'White' & `Officer five ethnicity` == 'Hispanic'] <- "Hispanic" )

#recombine them into one row to make groups
UOF_officerGroupsTest %>% unite(Officers.Race, c(`Officer one Race`,`Officer two Race`,`Officer three Race`,`Officer four Race`,`Officer five Race`), sep = ";")

UOF_officerGroupsTest %>% 
  unite(Officers.Ethnicity, c('Officer one ethnicity', 'Officer two ethnicity', 'Officer three ethnicity','Officer four ethnicity', 'Officer five ethnicity'), sep = ";")

#function to make it so mixed race groups say mixed and single race groups say that race in the Officer.Race.Groups col
f <- function(x) ifelse(length(u <- unique(unlist((strsplit(x, ";"))))) > 1, "Mixed", u)

UOF_officerGroupsTest = transform(UOF_officerGroupsTest, Officer.Race.Groups = Vectorize(f)(Officer.Race.Groups))

ggplot(UOF_officerGroups, aes(Officer.Race.Groups)) +
  geom_bar()





### Keeping just in case
UOF_officerGroupsTest <-separate(UOF_officerGroupsTest, Officers.Race, c('Officer one Race', 'Officer two Race', 'Officer three Race','Officer four Race', 'Officer five Race'), sep=";")
UOF_officerGroupsTest <-separate(UOF_officerGroupsTest, Officers.Ethnicity, c('Officer one ethnicity', 'Officer two ethnicity', 'Officer three ethnicity','Officer four ethnicity', 'Officer five ethnicity'), sep=";")

UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer one Race`[`Officer one Race` == 'White' & `Officer one ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer two Race`[`Officer two Race` == 'White' & `Officer two ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer three Race`[`Officer three Race` == 'White' & `Officer three ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer four Race`[`Officer four Race` == 'White' & `Officer four ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_officerGroupsTest<- within(UOF_officerGroupsTest, `Officer five Race`[`Officer five Race` == 'White' & `Officer five ethnicity` == 'Hispanic'] <- "Hispanic" )

ggplot(UOF_officerGroupsTest, aes(`Officer one Race`)) +
  geom_bar()

ggplot(UOF_officerGroupsTest, aes(`Officer two Race`)) +
  geom_bar()

ggplot(UOF_officerGroupsTest, aes(`Officer three Race`)) +
  geom_bar()

ggplot(UOF_officerGroupsTest, aes(`Officer four Race`)) +
  geom_bar()

ggplot(UOF_officerGroupsTest, aes(`Officer five Race`)) +
  geom_bar()








#############################
Shootings<- Shootings %>% separate_rows(Suspect.Race, Suspect.Gender, Suspect.Hit, Fatal, sep= ",")

ggplot(Shootings, aes(Suspect.Race)) +
  geom_bar()

ggplot(Shootings, aes(Suspect.Gender)) +
  geom_bar()

ggplot(Shootings, aes(Suspect.Hit)) +
  geom_bar()

ggplot(Shootings, aes(Fatal)) +
  geom_bar()

#some comparisons
ggplot(Shootings,
       aes(x = Suspect.Race,
           fill = Suspect.Hit))+
  geom_bar(position = "dodge")

ggplot(Shootings,
       aes(x = Suspect.Race,
           fill = Fatal))+
  geom_bar(position = "dodge")
