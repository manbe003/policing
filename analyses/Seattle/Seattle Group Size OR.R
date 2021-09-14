#loading libraries
library(here)
library(epitools)
library(tidyverse)
library(dplyr)
library(tidyr)

#loading in datasets
UOF<- read.csv(file=here('clean data/seattle/UseOfForce_Seattle.csv'), stringsAsFactors = FALSE)

#making date and time one combined column to later match rows based on the same date and time
UOF2<- UOF
UOF2$DateTime <- paste(UOF2$date, " ", UOF2$time)

#collapsing the Officer ID row so it puts each officer ID with the same incident DateTime in the same row seperated by commas
UOF3 <- UOF2 %>%
  dplyr::group_by(DateTime) %>%
  summarise(Officer_ID = paste(Officer_ID, collapse=",  "))

#collapsing the Force Type row so it puts each officer ID with the same incident DateTime in the same row seperated by commas
UOF4<- UOF2 %>%
  dplyr::group_by(DateTime) %>%
  summarise(Incident_Type= paste(Incident_Type, collapse=",  "))

#combing them so all the needed data is in one dataframe
UOF3$IncidentType <- paste(UOF4$Incident_Type)  

#Binning Force Type (1- no weapon, 2- non lethal weapon, 3- lethal force)
UOF_All_FixLevels <- UOF3
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`IncidentType` = case_when(
    str_detect(`IncidentType`, "Level 3 - Use of Force") ~ "3",
    str_detect(`IncidentType`, "Level 3 - OIS") ~ "3",
    str_detect(`IncidentType`, "Level 2 - Use of Force") ~ "2",
    str_detect(`IncidentType`, "Level 1 - Use of Force") ~ "1",
    TRUE ~ `IncidentType`
  ))



#binning number of officers into 1,2, or 3+
OfficerBinning <- function (dataframe, dataframecol, separator){
  dataframe['Number of Officers'] <- NA
  dataframe['Binning Number of Officers'] <- NA
  dataframe$`Number of Officers` <- str_count(dataframecol, coll(separator))+1
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="1"]<- "1"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="2"]<- "2"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers` > "2"]<- "3+"
  return(dataframe)
  
}

UOF_All_FixLevels<- OfficerBinning(UOF_All_FixLevels,UOF_All_FixLevels$Officer_ID, ",  ")




OR_Prep = function(dataset,column){
  #making a column binning level of force as lethal vs non lethal
  dataset['Lethal.vs.Non-lethal.Weapon'] <- column
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('1|2', 'Non-Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('3', 'Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  
  #making a column binning level of force as Weapon vs No Weapon
  dataset['Weapon.vs.No Weapon'] <- column
  dataset$`Weapon.vs.No Weapon`<- gsub('2|3', 'Weapon', dataset$`Weapon.vs.No Weapon`)
  dataset$`Weapon.vs.No Weapon`<- gsub('1', 'No Weapon', dataset$`Weapon.vs.No Weapon`)
  return(dataset)
}

UOF_All_FixLevels <- OR_Prep(UOF_All_FixLevels,UOF_All_FixLevels$IncidentType)


#2nd function (referring to which variable you want to be the rows and columns of the odds ratio)
OR_Function = function(row,column1,column2){
  
  #making a table of Lethal vs non lethal used with each race
  OR_Table<-table(row, column1)
  OR_Table <- OR_Table[c(3,1:2),]
  print(OR_Table)
  
  ###Odds Ratio
  OR<-oddsratio(OR_Table)
  #printing the outcome so its easier to read
  print(OR$measure)
  print(OR$p.value)
  
  #making a table of Weapon vs No Weapon used with each race
  OR_Table2<-table(row, column2)
  OR_Table2 <- OR_Table2[c(3,1:2),]
  OR_Table2<- OR_Table2[,c(2,1)]
  print(OR_Table2)
  
  ###Odds Ratio
  OR2<-oddsratio(OR_Table2)
  #printing the outcome so its easier to read
  print(OR2$measure)
  print(OR2$p.value) 
  
  
}

OR_Function(UOF_All_FixLevels$`Binning Number of Officers`,UOF_All_FixLevels$`Lethal.vs.Non-lethal.Weapon`,UOF_All_FixLevels$`Weapon.vs.No Weapon`)

ggplot(UOF_All_FixLevels,
       aes(x = `Binning Number of Officers`,
           fill = as.character(IncidentType)))+
  geom_bar(position = "dodge")


ggplot(UOF_All_FixLevels,
       aes(x = `Binning Number of Officers`,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_All_FixLevels,
       aes(x = `Binning Number of Officers`,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")


