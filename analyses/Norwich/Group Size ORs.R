#loading libraries
library(here)
library(epitools)
library(tidyverse)

#loading datasets
UOF<- read.csv(file=here('clean data/Norwich/norwich_UOF.csv'), stringsAsFactors = FALSE)

#Binning Force Type (1- no weapon, 2- non lethal weapon, 3- lethal force) 
##binning displaying a weapon as 1 (rather than 3 or 2)
UOF_All_FixLevels1 <- UOF
UOF_All_FixLevels1 <- UOF_All_FixLevels1 %>%
  mutate(`PD_force_type` = case_when(
    str_detect(`PD_force_type`, "Discharged") ~ "3",
    str_detect(`PD_force_type`, "Taser Deployment") ~ "2",
    str_detect(`PD_force_type`, "OC Spray") ~ "2",
    str_detect(`PD_force_type`, "O.C. Spray") ~ "2",
    str_detect(`PD_force_type`, "K9 bite") ~ "2",
    str_detect(`PD_force_type`, "Displayed") ~ "1",
    str_detect(`PD_force_type`, "displayed") ~ "1",
    str_detect(`PD_force_type`, "Forced to Ground") ~ "1",
    str_detect(`PD_force_type`, "Forced to ground") ~ "1",
    str_detect(`PD_force_type`, "Strike") ~ "1",
    str_detect(`PD_force_type`, "Escort position") ~ "1",
    str_detect(`PD_force_type`, "escort position") ~ "1",
    str_detect(`PD_force_type`, "Escort Position") ~ "1",
    str_detect(`PD_force_type`, "strike") ~ "1",
    str_detect(`PD_force_type`, "Physical Force") ~ "1",
    str_detect(`PD_force_type`, "Physical force") ~ "1",
    str_detect(`PD_force_type`, "Grapple/Wrestle") ~ "1",
    TRUE ~ `PD_force_type`
  ))

##Binning weapons displayed as 3 or 2 (rather than 1)
UOF_All_FixLevels3 <- UOF
UOF_All_FixLevels3 <- UOF_All_FixLevels3 %>%
  mutate(`PD_force_type` = case_when(
    str_detect(`PD_force_type`, "Firearm") ~ "3",
    str_detect(`PD_force_type`, "Taser") ~ "2",
    str_detect(`PD_force_type`, "OC Spray") ~ "2",
    str_detect(`PD_force_type`, "O.C. Spray") ~ "2",
    str_detect(`PD_force_type`, "K9") ~ "2",
    str_detect(`PD_force_type`, "K-9") ~ "2",
    str_detect(`PD_force_type`, "Forced to Ground") ~ "1",
    str_detect(`PD_force_type`, "Forced to ground") ~ "1",
    str_detect(`PD_force_type`, "Strike") ~ "1",
    str_detect(`PD_force_type`, "Escort position") ~ "1",
    str_detect(`PD_force_type`, "escort position") ~ "1",
    str_detect(`PD_force_type`, "Escort Position") ~ "1",
    str_detect(`PD_force_type`, "strike") ~ "1",
    str_detect(`PD_force_type`, "Physical Force") ~ "1",
    str_detect(`PD_force_type`, "Physical force") ~ "1",
    str_detect(`PD_force_type`, "Grapple/Wrestle") ~ "1",
    TRUE ~ `PD_force_type`
  ))






#Binning Number of Officers for Fixlevels 1
UOF_All_FixLevels1['Binning Number of Officers'] <- NA
UOF_All_FixLevels1$`Binning Number of Officers`[UOF_All_FixLevels1$number_of_officers=="1"]<- "1"
UOF_All_FixLevels1$`Binning Number of Officers`[UOF_All_FixLevels1$number_of_officers=="2"]<- "2"
UOF_All_FixLevels1$`Binning Number of Officers`[UOF_All_FixLevels1$number_of_officers > "2"]<- "3+"

#Binning Number of Officers for Fixlevels 3
UOF_All_FixLevels3['Binning Number of Officers'] <- NA
UOF_All_FixLevels3$`Binning Number of Officers`[UOF_All_FixLevels3$number_of_officers=="1"]<- "1"
UOF_All_FixLevels3$`Binning Number of Officers`[UOF_All_FixLevels3$number_of_officers=="2"]<- "2"
UOF_All_FixLevels3$`Binning Number of Officers`[UOF_All_FixLevels3$number_of_officers > "2"]<- "3+"




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

UOF_All_FixLevels1 <- OR_Prep(UOF_All_FixLevels1,UOF_All_FixLevels1$PD_force_type)
UOF_All_FixLevels3 <- OR_Prep(UOF_All_FixLevels3,UOF_All_FixLevels3$PD_force_type)


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

OR_Function(UOF_All_FixLevels1$`Binning Number of Officers`,UOF_All_FixLevels1$`Lethal.vs.Non-lethal.Weapon`,UOF_All_FixLevels1$`Weapon.vs.No Weapon`)
OR_Function(UOF_All_FixLevels3$`Binning Number of Officers`,UOF_All_FixLevels3$`Lethal.vs.Non-lethal.Weapon`,UOF_All_FixLevels3$`Weapon.vs.No Weapon`)



#Doing Weapon vs. No Weapon OR separately because Lethality OR doesnt work for Fixlevels 1
OR_Table2<-table(UOF_All_FixLevels1$`Binning Number of Officers`, UOF_All_FixLevels1$`Weapon.vs.No Weapon`)
OR_Table2 <- OR_Table2[c(3,1:2),]
OR_Table2<- OR_Table2[,c(2,1)]
print(OR_Table2)

###Odds Ratio
OR2<-oddsratio(OR_Table2)
#printing the outcome so its easier to read
print(OR2$measure)
print(OR2$p.value) 




#graphs

ggplot(UOF_All_FixLevels,
       aes(x = `Binning Number of Officers`,
           fill = as.character(PD_force_type)))+
  geom_bar(position = "dodge")


ggplot(UOF_All_FixLevels,
       aes(x = `Binning Number of Officers`,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_All_FixLevels,
       aes(x = `Binning Number of Officers`,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")
