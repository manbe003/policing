#loading libraries
library(here)
library(epitools)
library(tidyverse)

#loading datasets
UOF<- read.csv(file=here('clean data/Norwich/norwich_UOF.csv'), stringsAsFactors = FALSE)

#Binning Force Type (1- no weapon, 2- non lethal weapon, 3- lethal force) 
##binning displaying a weapon as 1 (rather than 3 or 2)
UOF_All_FixLevels <- UOF
UOF_All_FixLevels <- UOF_All_FixLevels %>%
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



#Binning Number of Officers
UOF_All_FixLevels['Binning Number of Officers'] <- NA
UOF_All_FixLevels$`Binning Number of Officers`[UOF_All_FixLevels$number_of_officers=="1"]<- "1"
UOF_All_FixLevels$`Binning Number of Officers`[UOF_All_FixLevels$number_of_officers=="2"]<- "2"
UOF_All_FixLevels$`Binning Number of Officers`[UOF_All_FixLevels$number_of_officers > "2"]<- "3+"


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

UOF_All_FixLevels <- OR_Prep(UOF_All_FixLevels,UOF_All_FixLevels$PD_force_type)



#Doing Weapon vs. No Weapon OR separately because Lethality OR doesnt work for Fixlevels bc it is 0 in two of the three rows in column 1
OR_Table2<-table(UOF_All_FixLevels$`Binning Number of Officers`, UOF_All_FixLevels$`Weapon.vs.No Weapon`)
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
