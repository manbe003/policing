#Bloomington OR Script

#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
PackageDependency()

install.packages("sqldf")
library(sqldf)
library(epitools)
library(ggplot2)

#loading dataset
UOF<- read.csv(file = 'clean data/Bloomington/UOF.csv', stringsAsFactors = FALSE)


#sqldf doesnt like periods in the name so we're changing it
colnames(UOF) <- gsub("\\.","_",colnames(UOF))

#making a dataframe based on the rows with the same Event and Subjects
#counting the number of times the same subject is under the same Event ID, giving us the number of officers
#also making sure we have the max force level for each Event id

UOF_OfficerCount <- sqldf("SELECT 
      Event, Suspect_Condition, Suspect_Age, Suspect_Gender, Suspect_Race, Suspect_Armed, Suspect_Injured, MAX(Force_Level), COUNT(*)  
      FROM UOF
      GROUP BY Event, Event, Suspect_Condition, Suspect_Age, Suspect_Gender, Suspect_Race, Suspect_Armed, Suspect_Injured
      HAVING COUNT(*)>1")
colnames(UOF_OfficerCount)[9]<- "Number_of_Officers"
colnames(UOF_OfficerCount)[8]<- "Max_Force_Level"

#making a seperate dataframe with the columns needed to merge Officercount dataframe back into this
UOF2<- UOF[, c("Event", "Suspect_Condition", "Suspect_Age", "Suspect_Gender", "Suspect_Race", "Suspect_Armed", "Suspect_Injured", "Force_Level")]
UOF2<- UOF2[!(UOF2$Event %in% UOF_OfficerCount$Event),]

colnames(UOF2)[8]<- "Max_Force_Level"
#we know if it is in this dataset then there was only one officer so we can make this column all 1s
UOF2[,"Number_of_Officers"] <- "1"


#rbinding the dataframes together and deleting an empty row
New_UOF<- rbind(UOF2, UOF_OfficerCount)
New_UOF<- New_UOF[-c(1285), ] 

#Binning Number of Officers 
New_UOF["Binning_Number_of_Officers"] <- NA
New_UOF$Binning_Number_of_Officers[New_UOF$Number_of_Officers=="1"]<- "1"
New_UOF$Binning_Number_of_Officers[New_UOF$Number_of_Officers=="2"]<- "2"
New_UOF$Binning_Number_of_Officers[New_UOF$Number_of_Officers > "2"]<- "3+"


###Function to prepare the Dataset for the OR
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

New_UOF <- OR_Prep(New_UOF,New_UOF$Max_Force_Level)

#doing OR without function because it had 0s as values in the lethal vs non lethal table
  #making a table of Lethal vs non lethal used with each race
  OR_Table<-table(New_UOF$Binning_Number_of_Officers, New_UOF$`Weapon.vs.No Weapon`)
  OR_Table <- OR_Table[c(3,1:2),]
  print(OR_Table)
  
  ###Odds Ratio
  OR<-oddsratio(OR_Table)
  #printing the outcome so its easier to read
  print(OR$measure)
  print(OR$p.value)
  
  
  
#some graphs comparing force level and group size

  ggplot(New_UOF,
         aes(x = Binning_Number_of_Officers,
             fill = as.character(Max_Force_Level)))+
    geom_bar(position = "dodge")
    
  ggplot(New_UOF,
         aes(x = Binning_Number_of_Officers,
             fill = `Lethal.vs.Non-lethal.Weapon`))+
    geom_bar(position = "dodge")
  
  ggplot(New_UOF,
         aes(x = Binning_Number_of_Officers,
             fill = `Weapon.vs.No Weapon`))+
    geom_bar(position = "dodge")

  
  
  