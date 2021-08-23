#loading libraries
library(here)
library(epitools)
library(tidyverse)


#loading datasets
UOF<-read.csv(file=here('clean data/Northampton/Northampton UOF.csv'), stringsAsFactors = FALSE)

OR_Function = function(dataset,row,column){
  
  #making a column binning level of force as lethal vs non lethal
  dataset['Lethal.vs.Non-lethal.Weapon'] <- column
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('1|2', 'Non-Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('3', 'Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  
  #making a column binning level of force as Weapon vs No Weapon
  dataset['Weapon.vs.Weapon'] <- column
  dataset$`Weapon.vs.Weapon`<- gsub('2|3', 'Weapon', dataset$`Weapon.vs.Weapon`)
  dataset$`Weapon.vs.Weapon`<- gsub('1', 'No Weapon', dataset$`Weapon.vs.Weapon`)
  
  #making a table of Lethal vs non lethal used with each race
  OR_Table<-table(row, dataset$`Lethal.vs.Non-lethal`)
  OR_Table <- OR_Table[c(3,1:2),]
  print(OR_Table)
  
  ###Odds Ratio
  OR<-oddsratio(OR_Table)
  #printing the outcome so its easier to read
  print(OR$measure)
  print(OR$p.value)
  
  #making a table of Weapon vs No Weapon used with each race
  OR_Table2<-table(row, dataset$`Weapon.vs.Weapon`)
  OR_Table2 <- OR_Table2[c(3,1:2),]
  OR_Table2<- OR_Table2[,c(2,1)]
  print(OR_Table2)
  
  ###Odds Ratio
  OR2<-oddsratio(OR_Table2)
  #printing the outcome so its easier to read
  print(OR2$measure)
  print(OR2$p.value) 
  
  
}

OR_Function(UOF,UOF$Binning.Number.of.Officers,UOF$PD.Force.Type)



## if we want the columns made in the previous function to actually be added to the dataset we do this function first and then the next one to get the same result as the funtion above
OR_Prep = function(dataset,rcolumn){
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

UOF <- OR_Prep(UOF,UOF$PD.Force.Type)


#2nd function (referring to which variable you want to be the rows and columns of the odds ratio)
OR_Function2 = function(row,column1,column2){
  
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



