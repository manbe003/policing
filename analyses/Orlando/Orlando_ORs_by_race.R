##ORs split by race for Orlando
##Can't be finished until we figure out what's going on with the Orlando database

#loading libraries
library(here)
library(epitools)
library(tidyverse)
library(dplyr)
library(tidyr)

#loading in datasets
UOF <- read.csv(file = here("clean data/Orlando/UOF (cleaned).csv"), stringsAsFactors = FALSE)


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

UOF <- OR_Prep(UOF,UOF$UOF.Level)

#now need to make suspect race W/NW
#first: how should I handle cases with multiple suspects?
#repeat rows? Only use if all offenders are same race? Never use for multiple offenders? Use minority race?
#first I'll just remove them

UOF$Offenders.Race=gsub(".*;.*", NA, UOF$Offenders.Race)


#next, add hispanic as a subject race

for (i in 1:length(UOF$Offenders.Ethnicity)){
  if(UOF$Offenders.Ethnicity[i] == "Hispanic"){
    UOF$Offenders.Race[i]<-"Hispanic"
  }
}

UOF$wnw<-UOF$Offenders.Race
UOF <- UOF %>%
  mutate(`wnw` = case_when(
    str_detect(`wnw`, "Black") ~ "Nonwhite",
    str_detect(`wnw`, "White") ~ "White",
    str_detect(`wnw`, "Hispanic") ~ "Nonwhite",
    str_detect(`wnw`, "Asian") ~ "Nonwhite",
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


UOF_split<-split(UOF, UOF$wnw)
OR_Function(UOF_split$Nonwhite$`Binning.Number.of.Officers`, UOF_split$Nonwhite$UOF.Level)
OR_Function(UOF_split$White$`Binning.Number.of.Officers`, UOF_split$White$UOF.Level)
#BUT this is misleading because it does not seem to include shootings. Discuss

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = Weapon.vs.Weapon))+
  geom_bar(position = "dodge")
