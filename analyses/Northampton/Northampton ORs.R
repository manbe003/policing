#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())

#loading datasets
UOF<-read.csv(file=here('clean data/Northampton/Northampton UOF.csv'), stringsAsFactors = FALSE)


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

UOF <- OR_Prep(UOF,UOF$Force.Binning)



##Function doesnt work because it creates an Error when it sees there is a 0 in the Lethality table so doing it step by step
#making a table of Lethal vs non lethal used with each race
OR_Table<-table(UOF$Binning.Number.of.Officers, UOF$`Lethal.vs.Non-lethal.Weapon`)
OR_Table <- OR_Table[ c(3,1:2),]
OR_Table <- OR_Table[- c(3),]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)

#making a table of Weapon vs No Weapon used with each race
OR_Table2<-table(UOF$Binning.Number.of.Officers, UOF$`Weapon.vs.No Weapon`)
OR_Table2 <- OR_Table2[c(3,1:2),]
OR_Table2<- OR_Table2[,c(2,1)]
print(OR_Table2)

###Odds Ratio
OR2<-oddsratio(OR_Table2)
#printing the outcome so its easier to read
print(OR2$measure)
print(OR2$p.value)



#graphs

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = as.character(Force.Binning)))+
  geom_bar(position = "dodge")

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")


