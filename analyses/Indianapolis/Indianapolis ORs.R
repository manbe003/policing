#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()

#loading in datasets
UOF<- read.csv(file = here("clean data/Indianapolis/UOF.csv"), stringsAsFactors = FALSE)
Shootings<- read.csv(file = here("clean data/Indianapolis/OIS.csv"), stringsAsFactors = FALSE)



UOF_Function<- UOF
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

UOF_Function<- OR_Prep(UOF_Function, UOF_Function$ForceBinning)

#splitting them into their own dataframes to compute lethal vs non lethal and weapon vs no weapon seperatly
UOF_Lethal<- UOF_Function
UOF_Weapon<- UOF_Function

##Weapon vs No weapon OR##

#removing races with 0's from UOF_weapon to compute OR
UOF_Weapon$residentRace<- gsub('N/A|Polynesian', NA, UOF_Weapon$residentRace)

#making a table of Weapon vs No Weapon used with each race
OR_Table<-table(UOF_Weapon$residentRace, UOF_Weapon$`Weapon.vs.No Weapon`)
OR_Table <- OR_Table[c(6,1:5),]
OR_Table<- OR_Table[,c(2,1)]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value) 

#graph
ggplot(UOF_Weapon,
       aes(x = residentRace,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")

## Lethal vs non Lethal OR ##
#taking out races with 0's to compute OR
UOF_Lethal$residentRace<- gsub('Asian|Hispanic|Native American|Polynesian', NA, UOF_Lethal$residentRace)


#making a table of Lethal vs Non Lethal with each race
OR_Table2<-table(UOF_Lethal$residentRace, UOF_Lethal$`Lethal.vs.Non-lethal.Weapon`)
OR_Table2 <- OR_Table2[c(3,1:2),]
print(OR_Table2)

###Odds Ratio
OR2<-oddsratio(OR_Table2)
#printing the outcome so its easier to read
print(OR2$measure)
print(OR2$p.value) 

#graph
ggplot(UOF_Lethal,
       aes(x = residentRace,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")



###Shootings OR
Condition<- as.data.frame(na.omit(cbind(as.character(Shootings$residentCondition), as.character(Shootings$residentRace))))
Condition$V1 <- gsub('Death|Gunshot Wound','Shot and or Killed', Condition$V1)
Condition$V1 <- gsub('N/A',NA, Condition$V1)
Condition$V2 <- gsub('N/A',NA, Condition$V2)

Race.Condition<- table(Condition$V2, Condition$V1)
Race.Condition <- Race.Condition[c(3,1:2),]
print(Race.Condition)

Condition_OR<-oddsratio(Race.Condition)
#printing the outcome so its easier to read
print(Condition_OR$measure)
print(Condition_OR$p.value)

ggplot(Condition,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")






