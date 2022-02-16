library(here)
library(epitools)
library(tidyverse)


UOF <- read.csv(file=here('clean data/New Orleans/New Orleans UOF.csv'), stringsAsFactors = FALSE)

UOF_All_FixLevels <- UOF
UOF_All_FixLevels <- UOF_All_FixLevels %>%
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

UOF_All_FixLevels <- OR_Prep(UOF_All_FixLevels,UOF_All_FixLevels$Force.Type)

OfficerBinning <- function (dataframe, dataframecol, separator){
  dataframe['Number of Officers'] <- NA
  dataframe['Binning Number of Officers'] <- NA
  dataframe$`Number of Officers` <- str_count(dataframecol, coll(separator))+1
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="1"]<- "1"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="2"]<- "2"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers` > "2"]<- "3+"
  return(dataframe)
  
}

UOF_All_FixLevels<-OfficerBinning(UOF_All_FixLevels,UOF_All_FixLevels$Officer.Race, "|")
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`Subject.Race` = case_when(
    str_detect(`Subject.Race`, "Black") ~ "Black",
    str_detect(`Subject.Race`, "Indian") ~ "Native",
    str_detect(`Subject.Race`, "Hispanic") ~ "Hispanic",
    str_detect(`Subject.Race`, "Asian") ~ "Asian",
    str_detect(`Subject.Race`, "Hawaiian") ~ "Pacific Islander",
    str_detect(`Subject.Race`, "White") ~ "White",
    str_detect(`Subject.Race`, "W") ~ "White",
    TRUE ~ `Subject.Race`
  ))

UOF_All_FixLevels$Subject.Race=gsub(".*Unknown.*", NA, UOF_All_FixLevels$Subject.Race)

UOF_All_FixLevels$wnw<-UOF_All_FixLevels$Subject.Race
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`wnw` = case_when(
    str_detect(`wnw`, "Black") ~ "Nonwhite",
    str_detect(`wnw`, "White") ~ "White",
    str_detect(`wnw`, "Native") ~ "Nonwhite",
    str_detect(`wnw`, "Hispanic") ~ "Nonwhite",
    str_detect(`wnw`, "Asian") ~ "Nonwhite",
    str_detect(`wnw`, "Pacific") ~ "Nonwhite",
    TRUE ~ `wnw`
  ))

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

UOF_split<-split(UOF_All_FixLevels,UOF_All_FixLevels$wnw)
OR_Function(UOF_split$Nonwhite$Binning.Number.of.Officers,UOF_split$Nonwhite$`Lethal.vs.Non-lethal.Weapon`,UOF_split$Nonwhite$`Weapon.vs.No Weapon`)
OR_Function(UOF_split$White$Binning.Number.of.Officers,UOF_split$White$`Weapon.vs.No Weapon`, UOF_split$White$Force.Type)


ggplot(UOF_All_FixLevels,
       aes(x = Binning.Number.of.Officers,
           fill = as.character(Force.Type)))+
  geom_bar(position = "dodge")

ggplot(UOF_All_FixLevels,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_All_FixLevels,
       aes(x = Binning.Number.of.Officers,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")
