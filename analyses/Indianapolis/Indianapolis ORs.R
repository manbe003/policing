#libraries
library(here)
library(tidyverse)
library(epitools)
library(tidyr)
library(dplyr)

#loading in datasets
UOF<- read.csv(file = here("clean data/Indianapolis/UOF.csv"), stringsAsFactors = FALSE)
Shootings<- read.csv(file = here("clean data/Indianapolis/OIS.csv"), stringsAsFactors = FALSE)


UOF_levels<- UOF
UOF_levels$officerForceType<- gsub('Physical-Kick|Physical-Hands, Fist, Feet|Physical-Weight Leverage|Physical-Take Down|Physical-Palm Strike|Physical-Elbow Strike|Physical-Handcuffing|Physical-Leg Sweep|Physical-Knee Strike|Physical-Push|Physical-Other|Physical-Joint/Pressure|Physical-Fist Strike','Physical',UOF_levels$officerForceType)
UOF_levels$officerForceType<- gsub('Less Lethal-Taser|Less Lethal-Personal CS/OC spray|Less Lethal-Baton|Less Lethal-Burning CS|Less Lethal-Flash Bang|Less Lethal-Pepperball|Less Lethal-Bps Gas|Less Lethal-CS Grenade|Less Lethal-Other|Less Lethal-CS/OC|Less Lethal-Clearout OC|Less Lethal-Bean Bag|Less Lethal-CS Fogger|Canine Bite','Less Than Lethal',UOF_levels$officerForceType)
UOF_levels$officerForceType<- gsub('Lethal-Handgun|Lethal-Vehicle','Lethal',UOF_levels$officerForceType)
UOF_levels$officerForceType<- gsub('N/A',NA,UOF_levels$officerForceType)

#to compare Lethal vs less than lethal I have to make these races NA bc they have 0s in the lethal column
UOF_levels2<- UOF_levels
UOF_levels2$residentRace<- gsub('N/A|Asian|Hispanic|Native American|Polynesian', NA, UOF_levels2$residentRace)
UOF_levels2$officerForceType<- gsub('Physical|Less Than Lethal','Less Than Lethal',UOF_levels2$officerForceType)


#taking out Polynesian has a 0 in Lethal and less than lethal column and combing less than lethal and lethal as worse outcome
UOF_levels$residentRace<- gsub('N/A|Polynesian', NA, UOF_levels$residentRace)
UOF_levels$officerForceType<- gsub('Lethal|Less Than Lethal','Less Than Lethal/Lethal',UOF_levels$officerForceType)


##First comparing Physical vs. Less than Lethal/Lethal
Race_ForceType<- as.data.frame(na.omit(cbind(as.character(UOF_levels$officerForceType), as.character(UOF_levels$residentRace))))

#table
Level.Race<-table(Race_ForceType$V2, Race_ForceType$V1)
Level.Race <- Level.Race[c(6,1:5),]
print(Level.Race)

#OR
Race_OR<-oddsratio(Level.Race)
#printing the outcome so its easier to read
print(Race_OR$measure)
print(Race_OR$p.value)

ggplot(Race_ForceType,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")


##comparing Lethal vs Less than lethal
Race_ForceType2<- as.data.frame(na.omit(cbind(as.character(UOF_levels2$officerForceType), as.character(UOF_levels2$residentRace))))


Level.Race2<-table(Race_ForceType2$V2, Race_ForceType2$V1)
Level.Race2 <- Level.Race[c(3,1:2),]
print(Level.Race2)

#OR
Race_OR2<-oddsratio(Level.Race2)
print(Race_OR2$measure)
print(Race_OR2$p.value)

ggplot(Race_ForceType2,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")


###Shootings OR
Condition<- as.data.frame(na.omit(cbind(as.character(Shootings$residentCondition), as.character(Shootings$residentRace))))
Condition$V1 <- gsub('Death|Gunshot Wound','Shot and or Killed', Condition$V1)
Condition$V1 <- gsub('N/A',NA, Condition$V1)
Condition$V2 <- gsub('N/A',NA, Condition$V2)

Race.Condition<- table(Condition$V2, Condition$V1)
Race.Condition <- Race.Condition[c(3,1:2),]
print(Race.Condition)

Condition_OR<-oddsratio(Race.Condition)
#printing the outcome so its easier to read
print(Condition_OR$measure)
print(Condition_OR$p.value)

ggplot(Condition,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")






