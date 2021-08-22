#libraries
library(here)
library(tidyr)
library(dplyr)
library(stringr)
library(epitools)
library(tidyverse)


#loading files
UOF <- read.csv(file = here("clean data/orlando/UOF (cleaned).csv"), stringsAsFactors = FALSE)
#making the types of UOF level 3 or 4, making nos NA, and removing Asian to make it only 2 races to be more simple for me
UOF_levels <- UOF
UOF_levels$Electronic.Device.Used[UOF_levels$Electronic.Device.Used=="Yes"]<-("4")
UOF_levels$Chemical.Agent.Used[UOF_levels$Chemical.Agent.Used=="Yes"]<-("4")
UOF_levels$Tackle.Take.Down[UOF_levels$Tackle.Take.Down=="Yes"]<-("3")
UOF_levels$Impact.Weapons.Used[UOF_levels$Impact.Weapons.Used=="Yes"]<-("4")
UOF_levels$Physical.Strikes.Made[UOF_levels$Physical.Strikes.Made=="Yes"]<-("3")
UOF_levels$Deflation.Devices.Used[UOF_levels$Deflation.Devices.Used=="Yes"]<-("4")
UOF_levels$K9.Unit.Involved[UOF_levels$K9.Unit.Involved=="Yes"]<-("4")
UOF_levels[UOF_levels=="No"]<-NA


#making one row with UOF used
UOF_levels<- UOF_levels %>% unite("UOF.Level", Electronic.Device.Used:K9.Unit.Involved, sep = ";", na.rm = TRUE, remove = TRUE)

#if there were multiple things used I made it the worse one
UOF_levels$UOF.Level<- gsub('3;4|4;3|4;4;3|4;3;4|4;4;4|4;4|4;4;3;3|4;3;3|4;4;3;4|4;4;3;4;3|4;4;4;3|3;3;4|3;4;4|4;3;4;3|4;3;4;3;4|3;4;3|4;4;3;4;3;4|3;4;3;4|4;4;3;3;4', '4', UOF_levels$UOF.Level)
UOF_levels$UOF.Level<- gsub('3;3','3',UOF_levels$UOF.Level)
UOF_levels[UOF_levels==""]<-NA

#making a table with just the victim race and level of UOF
VictimRace_UOFLevel <- as.data.frame(na.omit(cbind(as.character(UOF_levels$UOF.Level), as.character(UOF_levels$Offenders.Race))))
VictimRace_UOFLevel <- VictimRace_UOFLevel %>% separate_rows(V2, sep = ";")

#making a table of Level of force used with each race
Level.Race<-table(VictimRace_UOFLevel$V2, VictimRace_UOFLevel$V1)
Level.Race <- Level.Race[c(3,1:2),]
print(Level.Race)

###Odds Ratio
Race_OR<-oddsratio(Level.Race)
#printing the outcome so its easier to read
print(Race_OR$measure)
print(Race_OR$p.value)

#graph
ggplot(VictimRace_UOFLevel,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")


##Odds ratio for group sizes

#making the number of officers row either groups of 1, 2, or 3
UOF_Officers<- UOF_levels
UOF_Officers$Officers.Involved <- gsub('3|4|5|6|7|8|9|10|12','3+',UOF_Officers$Officers.Involved)

#making the number of officers row either groups of <2 or 3+ for another comparison
UOF_Officers2<- UOF_Officers
UOF_Officers2$Officers.Involved <- gsub("1|2",'<2',UOF_Officers2$Officers.Involved)


# comparing groups of 1, 2, or 3+
#making a dataframe of just the UOF levels and Group Sizes for comparing 1, 2, or 3+
UOF_GroupsandLevels <-as.data.frame(na.omit(cbind(as.character(UOF_Officers$UOF.Level), as.character(UOF_Officers$Officers.Involved))))

#making a table of Group size and UOF level
Levels.Groups<-table(UOF_GroupsandLevels$V2,UOF_GroupsandLevels$V1)
print(Levels.Groups)

#Odds Ratio of 1,2,3+
Groups.OR<-oddsratio(Levels.Groups)
print(Groups.OR$measure)
print(Groups.OR$p.value)

#Graph
ggplot(UOF_GroupsandLevels,
       aes(x = V2,
           fill = V1))+
  geom_bar(position = "dodge")



#comparing groups of <2 or 3+
UOF_Groupsandlevels2 <-as.data.frame(na.omit(cbind(as.character(UOF_Officers2$UOF.Level), as.character(UOF_Officers2$Officers.Involved))))

Levels.Groups2<-table(UOF_Groupsandlevels2$V2,UOF_Groupsandlevels2$V1)
print(Levels.Groups2)

###Odds Ratio of <2 and 3+
Groups.OR2<-oddsratio(Levels.Groups2)
print(Groups.OR2$measure)
print(Groups.OR2$p.value)

#Graph
ggplot(UOF_Groupsandlevels2,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")



#doing odds ratios for each UOF for each race and Groups
UOF_Types<- UOF
UOF_Types$Officers.Involved <- gsub('3|4|5|6|7|8|9|10|12','3+',UOF_Types$Officers.Involved)
#UOF_Types$Officers.Involved <- gsub("1|2",'<2',UOF_Types$Officers.Involved)
UOF_Types <- UOF_Types %>% separate_rows(Offenders.Race, sep = ";")


#Taser and Race to double check function
#Taser_Race <- as.data.frame(na.omit(cbind(as.character(UOF_Types$Offenders.Race), as.character(UOF_Types$Electronic.Device.Used))))
#Taser_RaceCounts<- table(Taser_Race$V1,Taser_Race$V2)
#Taser_RaceCounts <- Taser_RaceCounts[c(3,1:2),]
#Taser_RaceCounts <- Taser_RaceCounts[,c(2,1)]
#print(Taser_RaceCounts)

#TaserRace.OR<-oddsratio(Taser_RaceCounts)
#print(TaserRace.OR$measure)
#print(TaserRace.OR$p.value)

#ggplot(Taser_Race,
 #      aes(x = V2,
  #        fill = V1))+
  #geom_bar(position = "dodge")

##Taser and Groups to double check function
#Taser_Groups <- as.data.frame(na.omit(cbind(as.character(UOF_Types$Officers.Involved), as.character(UOF_Types$Electronic.Device.Used))))
#Taser_GroupsCounts<- table(Taser_Groups$V1,Taser_Groups$V2)
#print(Taser_GroupsCounts)

#TaserRace.OR<-oddsratio(Taser_GroupsCounts)
#print(TaserRace.OR$measure)
#print(TaserRace.OR$p.value)

#ggplot(Taser_Groups,
       #aes(x = V2,
           #fill = V1))+
  #geom_bar(position = "dodge")

Function_UOFType = function(Data){
  Type<- as.data.frame(na.omit(cbind(as.character(UOF_Types$Offenders.Race), as.character(Data))))
  TypeCounts<- table(Type$V1,Type$V2)
  TypeCounts <- TypeCounts[c(3,1:2),]
  print(TypeCounts)

  Type.OR<-oddsratio(TypeCounts)
  print(Type.OR$measure)
  print(Type.OR$p.value)

  ggplot(Type,
        aes(x = V2,
             fill = V1))+
   geom_bar(position = "dodge")
  
}

Function_TypesGroups = function(Data){
  Groups <- as.data.frame(na.omit(cbind(as.character(UOF_Types$Officers.Involved), as.character(Data))))
  GroupsCounts<- table(Groups$V1,Groups$V2)
  print(GroupsCounts)
  
  Group.OR<-oddsratio(GroupsCounts)
  print(Group.OR$measure)
  print(Group.OR$p.value)
  
  ggplot(Groups,
         aes(x = V2,
             fill = V1))+
    geom_bar(position = "dodge")
}

#Taser
Function_UOFType(Data = UOF_Types$Electronic.Device.Used)
Function_TypesGroups(Data = UOF_Types$Electronic.Device.Used)

#chemical Weapon
Function_UOFType(Data = UOF_Types$Chemical.Agent.Used)
Function_TypesGroups(Data = UOF_Types$Chemical.Agent.Used)

#Tackle
Function_UOFType(Data = UOF_Types$Tackle.Take.Down)
Function_TypesGroups(Data = UOF_Types$Tackle.Take.Down)

#Impact Weapon
Function_UOFType(Data = UOF_Types$Impact.Weapons.Used)
Function_TypesGroups(Data = UOF_Types$Impact.Weapons.Used)

#Physical Strikes
Function_UOFType(Data = UOF_Types$Physical.Strikes.Made)
Function_TypesGroups(Data = UOF_Types$Physical.Strikes.Made)

#Deflation Device
Function_UOFType(Data = UOF_Types$Deflation.Devices.Used)
Function_TypesGroups(Data = UOF_Types$Deflation.Devices.Used)

#K9
Function_UOFType(Data = UOF_Types$K9.Unit.Involved)
Function_TypesGroups(Data = UOF_Types$K9.Unit.Involved)



### Shootings OR
Shootings<- read.csv(file = here("clean data/orlando/shooting (cleaned).csv"), stringsAsFactors = FALSE)
Shootings_Offenders<- Shootings %>% separate_rows(Suspect.Race, Suspect.Gender, Suspect.Hit, Fatal, sep= ",")
Shootings_Offenders[Shootings_Offenders=="Self-Inflicted"]<-(NA)
Shootings_Offenders$Suspect.Race[Shootings_Offenders$Suspect.Race=="Other"]<-(NA)

#dataframe with the variables I want
Shooting_Outcome <-as.data.frame(na.omit(cbind(as.character(Shootings_Offenders$Fatal), as.character(Shootings_Offenders$Suspect.Race))))

#table of shooting outcomes and race
Outcomes<-table(Shooting_Outcome$V2,Shooting_Outcome$V1)
print(Outcomes)

###Odds Ratio Race and Fatality of shooting
Outcomes.OR<-oddsratio(Outcomes)
print(Outcomes.OR$measure)
print(Outcomes.OR$p.value)

#graph
ggplot(Shooting_Outcome,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")







