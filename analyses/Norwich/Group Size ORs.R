#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()


#loading datasets
UOF<- read.csv(file=here('clean data/Norwich/norwich_UOF.csv'), stringsAsFactors = FALSE)

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

UOF <- OR_Prep(UOF,UOF$ForceBinning)



#Doing Weapon vs. No Weapon OR separately because Lethality OR doesnt work for Fixlevels bc it is 0 in two of the three rows in column 1
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
           fill = as.character(ForceBinning)))+
  geom_bar(position = "dodge")


ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF,
       aes(x = Binning.Number.of.Officers,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")
