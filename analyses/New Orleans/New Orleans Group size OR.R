library(here)
library(epitools)
library(tidyverse)


UOF <- read.csv(file=here('clean data/New Orleans/New Orleans UOF.csv'), stringsAsFactors = FALSE)

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

UOF <- OR_Prep(UOF,UOF$Force.Type)


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

OR_Function(UOF$Binning.Number.of.Officers,UOF$`Lethal.vs.Non-lethal.Weapon`,UOF$Weapon.vs.Weapon)

ggplot(UOF,
       aes(x = Force.Type,
           fill = Binning.Number.of.Officers))+
  geom_bar(position = "dodge")

#this is how i want it to be shown but it is not working, don't know why
ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = Force.Type))+
  geom_bar(position = "dodge")


ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Weapon.vs.Weapon`))+
  geom_bar(position = "dodge")



