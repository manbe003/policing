#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)
library(here)

#call in UOF dataset
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = FALSE)

#analyze probability that if there is 1 or 2 officers they will escalate vs if there are 3
##probability is simply # time it escalates/total times, split by group. 

#making date and time one combined column to later match rows based on the same date and time
UOF2<- UOF
UOF2$DateTime <- paste(UOF2$date, " ", UOF2$time)

#collapsing the Officer ID row so it puts each officer ID with the same incident DateTime in the same row seperated by commas
UOF3 <- UOF2 %>%
  dplyr::group_by(DateTime) %>%
  summarise(Officer_ID = paste(Officer_ID, collapse=",  "))

#collapsing the Force Type row so it puts each officer ID with the same incident DateTime in the same row seperated by commas
UOF4<- UOF2 %>%
  dplyr::group_by(DateTime) %>%
  summarise(Incident_Type= paste(Incident_Type, collapse=",  "))

#combing them so all the needed data is in one dataframe
UOF3$IncidentType <- paste(UOF4$Incident_Type)

#Binning Force Type (1- no weapon, 2- non lethal weapon, 3- lethal force)
UOF_All_FixLevels <- UOF3
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`IncidentType` = case_when(
    str_detect(`IncidentType`, "Level 3 - Use of Force") ~ "3",
    str_detect(`IncidentType`, "Level 3 - OIS") ~ "3",
    str_detect(`IncidentType`, "Level 2 - Use of Force") ~ "2",
    str_detect(`IncidentType`, "Level 1 - Use of Force") ~ "1",
    TRUE ~ `IncidentType`
  ))
UOF_All_FixLevels$IncidentType<-as.numeric(UOF_All_FixLevels$IncidentType)


#binning number of officers into 1,2, or 3+
OfficerBinning <- function (dataframe, dataframecol, separator){
  dataframe['Number of Officers'] <- NA
  dataframe['Binning Number of Officers'] <- NA
  dataframe$`Number of Officers` <- str_count(dataframecol, coll(separator))+1
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="1"]<- "1"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="2"]<- "2"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers` > "2"]<- "3+"
  return(dataframe)
  
}

UOF_All_FixLevels<- OfficerBinning(UOF_All_FixLevels,UOF_All_FixLevels$Officer_ID, ",  ")

probability<-function(df, colnum){
x<-cbind(table(df[,colnum]))

print("odds of escalating to force of 3")
print(x[3,1]/sum(x[,1]))

print("odds of escalating past force of 1")
print(sum(x[2:3,1])/sum(x[,1]))
}

probability(UOF_All_FixLevels, 3)

UOF_FixLevels<-split(UOF_All_FixLevels, f=(UOF_All_FixLevels$`Binning Number of Officers`))
probability(UOF_FixLevels$`1`,3)
probability(UOF_FixLevels$`2`,3)
probability(UOF_FixLevels$`3`,3)
