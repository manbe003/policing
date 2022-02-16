#install packages
library(here)
library(epitools)
library(tidyverse)
library(dplyr)
library(tidyr)
library(sqldf)


UOF<- read.csv(file=here('clean data/Indianapolis/UOF.csv'), stringsAsFactors = FALSE)

#Making a Force Binning column and binning the force type used
UOF$ForceBinning = UOF$officerForceType

UOF_FixLevels <- UOF
UOF_FixLevels <- UOF_FixLevels %>%
  mutate(ForceBinning = case_when(
    str_detect(ForceBinning, "Lethal") ~ "3",
    str_detect(ForceBinning, "Less Lethal") ~ "3",
    str_detect(ForceBinning, "Canine") ~ "2",
    str_detect(ForceBinning, "Physical") ~ "1",
    TRUE ~ ForceBinning
  ))

#making N/As in the column regular NAs
UOF_FixLevels$ForceBinning[UOF_FixLevels$ForceBinning=="N/A"]<-NA

#Counting the numbe of officers by counting the number of distinct Officer IDs with the same case ID and making a DF
UOF_OfficerCount <- sqldf("SELECT 
      id, COUNT(DISTINCT officerIdentifier)
      FROM UOF_FixLevels
      GROUP BY id")
colnames(UOF_OfficerCount)[2]<- "NumberofOfficers"


#Finding the Max force used in each case and making a DF
UOF_MaxForce<- sqldf("SELECT
      id, MAX(ForceBinning)
      FROM UOF_FixLevels
      GROUP BY id")
colnames(UOF_MaxForce)[2]<- "MaxForce"

#Combing the two DFs to make one with all the needed data
UOF_OfficerForce<- sqldf("SELECT
      A.id,A.MaxForce, NumberofOfficers
      FROM UOF_MaxForce as A
      JOIN UOF_OfficerCount as B
      ON A.id = B.id")

UOF_race <- sqldf("SELECT 
      id, 
      FROM UOF_FixLevels
      GROUP BY id")
colnames(UOF_OfficerCount)[2]<- "NumberofOfficers"

race<-as.data.frame(cbind(UOF$id,UOF$residentRace))
colnames(race)<-cbind("id","suspect_race")
race<-race[!duplicated(race[,c('id')]),]

UOF_all<-left_join(UOF_OfficerForce,race)

#making a column binning number of officers
UOF_all['Binning Number of Officers'] <- NA
UOF_all$`Binning Number of Officers`[UOF_all$NumberofOfficers=="1+"]<- NA
UOF_all$`Binning Number of Officers`[UOF_all$NumberofOfficers=="1"]<- "1"
UOF_all$`Binning Number of Officers`[UOF_all$NumberofOfficers=="2"]<- "2"
UOF_all$`Binning Number of Officers`[UOF_all$NumberofOfficers > 2]<- "3+"

#group race into white/non-white

UOF_all$wnw<-UOF_all$suspect_race
UOF_all <- UOF_all %>%
  mutate(`wnw` = case_when(
    str_detect(`wnw`, "Black") ~ "Nonwhite",
    str_detect(`wnw`, "White") ~ "White",
    str_detect(`wnw`, "Native") ~ "Nonwhite",
    str_detect(`wnw`, "Hispanic") ~ "Nonwhite",
    str_detect(`wnw`, "Asian") ~ "Nonwhite",
    str_detect(`wnw`, "Pacific") ~ "Nonwhite",
    str_detect(`wnw`, "Polynesian") ~ "Nonwhite",
    str_detect(`wnw`, "Bi-Racial") ~ "Nonwhite",
    TRUE ~ `wnw`
  ))

#fix NAs
UOF_all$wnw=gsub(".*N/A.*", NA, UOF_all$wnw)


#function to prep dataset for the OR
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

UOF_all<-OR_Prep(UOF_all, UOF_all$MaxForce)

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

UOF_all_split<-split(UOF_all, UOF_all$wnw)
OR_Function(UOF_all_split$Nonwhite$`Binning Number of Officers`,UOF_all_split$Nonwhite$`Lethal.vs.Non-lethal.Weapon`, UOF_all_split$Nonwhite$`Weapon.vs.No Weapon`)


ggplot(UOF_all_split$Nonwhite,
       aes(x = `Binning Number of Officers`,
           fill = as.character(MaxForce)))+
  geom_bar(position = "dodge")


ggplot(UOF_all_split$Nonwhite,
       aes(x = `Binning Number of Officers`,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_all_split$Nonwhite,
       aes(x = `Binning Number of Officers`,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")


ggplot(UOF_all_split$White,
       aes(x = `Binning Number of Officers`,
           fill = as.character(MaxForce)))+
  geom_bar(position = "dodge")


ggplot(UOF_all_split$White,
       aes(x = `Binning Number of Officers`,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_all_split$White,
       aes(x = `Binning Number of Officers`,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")

