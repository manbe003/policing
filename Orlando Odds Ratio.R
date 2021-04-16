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
UOF_levels$Offenders.Race[UOF_levels$Offenders.Race=="Asian"]<-NA

#making one row with UOF used
UOF_levels<- UOF_levels %>% unite("UOF.Level", Electronic.Device.Used:K9.Unit.Involved, sep = ";", na.rm = TRUE, remove = TRUE)

#if there were multiple things used I made it the worse one
UOF_levels$UOF.Level<- gsub('3;4|4;3|4;4;3|4;3;4|4;4;4|4;4|4;4;3;3|4;3;3|4;4;3;4|4;4;3;4;3|4;4;4;3|3;3;4|3;4;4|4;3;4;3|4;3;4;3;4|3;4;3|4;4;3;4;3;4|3;4;3;4|4;4;3;3;4', '4', UOF_levels$UOF.Level)
UOF_levels$UOF.Level<- gsub('3;3','3',UOF_levels$UOF.Level)
UOF_levels[UOF_levels==""]<-NA

#making a table with just the victim race and level of UOF
VictimRace_UOFLevel <- as.data.frame(na.omit(cbind(as.character(UOF_levels$UOF.Level), as.character(UOF_levels$Offenders.Race))))
VictimRace_UOFLevel <- VictimRace_UOFLevel %>% separate_rows(V2, sep = ";")

#counting instances in each race
Victim_Levels<- as.data.frame(VictimRace_UOFLevel %>% group_by(V2,V1) %>% summarise(count = length(V1[!is.na(V1)])))
Race_Level<- VictimRace_UOFLevel %>% unite("Race and Level", V1:V2, sep = ", ")
Victim_Levels2<- as.data.frame(Race_Level %>% group_by(`Race and Level`) %>% summarise(count = length(`Race and Level`[!is.na(`Race and Level`)])))


Level.Race<-table(VictimRace_UOFLevel$V2, VictimRace_UOFLevel$V1)

Level.Race2<-matrix(Level.Race, nrow=2)
rownames(Level.Race2)<-c("Black", "White")
BW_OR<-oddsratio(Level.Race2)


#making the number of officers row <2 or 3+
UOF_Officers<- UOF_levels
UOF_Officers$Officers.Involved <- gsub('3|4|5|6|7|8|9|10|12','3+',UOF_Officers$Officers.Involved)
#UOF_Officers$Officers.Involved <- gsub("1|2",'<2',UOF_Officers$Officers.Involved)

#making a dataframe of just the UOF levels and Group Sizes
UOF_GroupsandLevels <-as.data.frame(na.omit(cbind(as.character(UOF_Officers$Officers.Involved), as.character(UOF_Officers$UOF.Level))))


Levels.Groups<-table(UOF_GroupsandLevels$V2,UOF_GroupsandLevels$V1)
print(table(UOF_GroupsandLevels$V2,UOF_GroupsandLevels$V1))

Level.Groups2<-matrix(Levels.Groups, nrow=2)
rownames(Level.Groups2)<-c("Level 3", "Level 4")
BW_OR2<-oddsratio(Level.Groups2)













