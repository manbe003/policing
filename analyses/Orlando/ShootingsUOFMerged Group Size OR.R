#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())


#loading dataset
combined<- read.csv(file = "clean data/orlando/UOFShootingsCombined.csv")

OR_Prep = function(dataset,column){
  #making a column binning level of force as lethal vs non lethal
  dataset['Lethal.vs.Non-lethal.Weapon'] <- column
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('1|2', 'Non-Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('3', 'Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  
  #making a column binning level of force as Weapon vs No Weapon
  dataset['Weapon.vs.No.Weapon'] <- column
  dataset$`Weapon.vs.No.Weapon`<- gsub('2|3', 'Weapon', dataset$`Weapon.vs.No.Weapon`)
  dataset$`Weapon.vs.No.Weapon`<- gsub('1', 'No Weapon', dataset$`Weapon.vs.No.Weapon`)
  return(dataset)
}

combined<- OR_Prep(combined,combined$UOF.Level)

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

OR_Function(combined$Binning.Number.of.Officers,combined$`Lethal.vs.Non-lethal.Weapon`,combined$Weapon.vs.No.Weapon )


#graphs


ggplot(combined,
       aes(x = Binning.Number.of.Officers,
           fill = Weapon.vs.No.Weapon))+
  geom_bar(position = "dodge")

ggplot(combined,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")
