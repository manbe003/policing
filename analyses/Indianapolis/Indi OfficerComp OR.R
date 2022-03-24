#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()

#Load in dataset
UOF<- read.csv(file = 'clean data/Indianapolis/UOF.csv', stringsAsFactors = FALSE)

#Counting the number of officers by counting the number of distinct Officer IDs with the same case ID and making a DF
UOF_OfficerCount <- sqldf("SELECT 
      id, COUNT(DISTINCT officerIdentifier), officerRace
      FROM UOF
      GROUP BY id")
colnames(UOF_OfficerCount)[2]<- "NumberofOfficers"


#Finding the Max force used in each case and making a DF
UOF_MaxForce<- sqldf("SELECT
      id, MAX(ForceBinning)
      FROM UOF
      GROUP BY id")
colnames(UOF_MaxForce)[2]<- "MaxForce"

#Combing the two DFs to make one with all the needed data
UOF_OfficerForce<- sqldf("SELECT
      A.id,A.MaxForce, B.NumberofOfficers, B. officerRace
      FROM UOF_MaxForce as A
      JOIN UOF_OfficerCount as B
      ON A.id = B.id")



#making a column binning number of officers
UOF_OfficerForce['Binning Number of Officers'] <- NA
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="1+"]<- NA
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="1"]<- "1"
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="2"]<- "2"
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers > 2]<- "3+"



#using Zoe's code to find percent white of officer groups
UOF_OfficerForce$officerRace <- trimws(UOF_OfficerForce$officerRace)
UOF_OfficerForce$officerRace <- stri_trans_totitle(UOF_OfficerForce$officerRace)
UOF_OfficerForce$white.officers <- str_count(UOF_OfficerForce$officerRace, 'White')
UOF_OfficerForce$percent.white <- UOF_OfficerForce$white.officers / UOF_OfficerForce$NumberofOfficers
UOF_OfficerForce$percent.white <- UOF_OfficerForce$percent.white * 100
UOF_OfficerForce$percent.white <- round(UOF_OfficerForce$percent.white, digits = 0)

#deleting single officer cases
UOF_OfficerForce$percent.white[UOF_OfficerForce$Number.of.Officers == 1] <- NA

##Binning it as 0%,1-99%, and 100% white groups
UOF_OfficerForce$Binning.Percent.White[UOF_OfficerForce$percent.white >=1 & UOF_OfficerForce$percent.white <=99 ] <- "1-99%"
UOF_OfficerForce$Binning.Percent.White[UOF_OfficerForce$percent.white == 0] <- "0%"
UOF_OfficerForce$Binning.Percent.White[UOF_OfficerForce$percent.white == 100] <- "100%"

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

UOF_OfficerForce <- OR_Prep(UOF_OfficerForce,UOF_OfficerForce$MaxForce)

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
  OR_Table2 <- OR_Table2[, c(2,1)]
  print(OR_Table2)
  
  ###Odds Ratio
  OR2<-oddsratio(OR_Table2)
  #printing the outcome so its easier to read
  print(OR2$measure)
  print(OR2$p.value) 
  
  
}

OR_Function(UOF_OfficerForce$Binning.Percent.White,UOF_OfficerForce$`Lethal.vs.Non-lethal.Weapon`,UOF_OfficerForce$`Weapon.vs.No Weapon`)

#graphs
ggplot(UOF_OfficerForce,
       aes(x = Binning.Percent.White,
           fill = as.character(MaxForce)))+
  geom_bar(position = "dodge")


ggplot(UOF_OfficerForce,
       aes(x = Binning.Percent.White,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_OfficerForce,
       aes(x = Binning.Percent.White,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

