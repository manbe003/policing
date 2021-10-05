library(here)
library(epitools)
library(tidyverse)


UOF <- read.csv(file=here('clean data/New Orleans/New Orleans UOF.csv'), stringsAsFactors = FALSE)

UOF_All_FixLevels1 <- UOF
UOF_All_FixLevels1 <- UOF_All_FixLevels1 %>%
  mutate(Force.Type = case_when(
    str_detect(Force.Type, "Discharged") ~ "3",
    str_detect(Force.Type, "Vehicle as Weapon") ~ "3",
    str_detect(Force.Type, "Escort Tech") ~ "2",
    str_detect(Force.Type, "CEW") ~ "2",
    str_detect(Force.Type, "Canine") ~ "2",
    str_detect(Force.Type, "Baton") ~ "2",
    str_detect(Force.Type, "NonTrad Impact Weapon") ~ "2",
    str_detect(Force.Type, "Pointed") ~ "1",
    str_detect(Force.Type, "Exhibited") ~ "1",
    str_detect(Force.Type, "Canine (No Bite)") ~ "1",
    str_detect(Force.Type, "Hands") ~ "1",
    str_detect(Force.Type, "Take Down") ~ "1",
    str_detect(Force.Type, "Takedown") ~ "1",
    str_detect(Force.Type, "Head Strike") ~ "1",
    str_detect(Force.Type, "Force") ~ "1",
    str_detect(Force.Type, "Handcuffed Subject") ~ "1",
    TRUE ~ Force.Type
  ))




#binning force type according to paper notes - displays as 3 or 2 (depending on what is being displayed) rather than 1 
UOF_All_FixLevels3 <- UOF
UOF_All_FixLevels3 <- UOF_All_FixLevels3 %>%
  mutate(Force.Type = case_when(
    str_detect(Force.Type, "Firearm") ~ "3",
    str_detect(Force.Type, "Rifle") ~ "3",
    str_detect(Force.Type, "Vehicle as Weapon") ~ "3",
    str_detect(Force.Type, "Shotgun") ~ "3",
    str_detect(Force.Type, "Escort Tech") ~ "2",
    str_detect(Force.Type, "CEW") ~ "2",
    str_detect(Force.Type, "Canine") ~ "2",
    str_detect(Force.Type, "Baton") ~ "2",
    str_detect(Force.Type, "NonTrad Impact Weapon") ~ "2",
    str_detect(Force.Type, "Hands") ~ "1",
    str_detect(Force.Type, "Take Down") ~ "1",
    str_detect(Force.Type, "Takedown") ~ "1",
    str_detect(Force.Type, "Head Strike") ~ "1",
    str_detect(Force.Type, "Force") ~ "1",
    str_detect(Force.Type, "Handcuffed Subject") ~ "1",
    TRUE ~ Force.Type
  ))





OR_Prep = function(dataset,column){
  #making a column binning level of force as lethal vs non lethal
  dataset['Lethal.vs.Non-lethal.Weapon'] <- column
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('1|2', 'Non-Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('3', 'Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  
  #making a column binning level of force as Weapon vs No Weapon
  dataset['Weapon.vs.Weapon'] <- column
  dataset$`Weapon.vs.Weapon`<- gsub('2|3', 'Weapon', dataset$`Weapon.vs.Weapon`)
  dataset$`Weapon.vs.Weapon`<- gsub('1', 'No Weapon', dataset$`Weapon.vs.Weapon`)
  return(dataset)
}

UOF_All_FixLevels1 <- OR_Prep(UOF_All_FixLevels1,UOF_All_FixLevels1$Force.Type)
UOF_All_FixLevels3 <- OR_Prep(UOF_All_FixLevels3,UOF_All_FixLevels3$Force.Type)


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

OR_Function(UOF_All_FixLevels1$Binning.Number.of.Officers,UOF_All_FixLevels1$`Lethal.vs.Non-lethal.Weapon`,UOF_All_FixLevels1$Weapon.vs.Weapon)
OR_Function(UOF_All_FixLevels3$Binning.Number.of.Officers,UOF_All_FixLevels3$`Lethal.vs.Non-lethal.Weapon`,UOF_All_FixLevels3$Weapon.vs.Weapon)


ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = as.character(Force.Type)))+
  geom_bar(position = "dodge")


ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Weapon.vs.Weapon`))+
  geom_bar(position = "dodge")