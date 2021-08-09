#loading libraries
library(here)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(stringi)
library(stringr)

#loading datasets
UOF <- read.csv(file=here('dirty data/New Orleans/NOPD_Use_of_Force_Incidents.csv'), stringsAsFactors = FALSE)

#making a table with relevant data
AllMetadata_UOF<- cbind.data.frame(UOF$PIB.File.Number,UOF$Date.Occurred,UOF$Disposition,UOF$Service.Type,UOF$Use.of.Force.Type,UOF$Use.of.Force.Level,UOF$Officer.Race.Ethnicity,UOF$Officer.Gender,UOF$Officer.Age,UOF$Officer.Years.of.Service,UOF$Subject.Gender,UOF$Subject.Ethnicity,UOF$Subject.Age,UOF$Subject.Injured,UOF$Use.of.Force.Reason,UOF$Officer.Injured, stringsAsFactors=FALSE)
colnames(AllMetadata_UOF)<-(c("PIB File Number","Date","Disposition","Service Type","Force Type","Force Level","Officer Race","Officer Gender","Officer Age","Officers Yrs of Service","Subject Gender","Subject Race","Subject Age","Subject Injured","UOF Reason","Officer Injured"))

#making blanks NA
AllMetadata_UOF_NA<- AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF_NA==""]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF_NA=="Other | Other | Other"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF_NA=="Other"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF_NA=="Other | Other"]<-NA

#making a column counting number of officers in a group based on a column and its separator
OfficerBinning <- function (dataframe, dataframecol, separator){
    dataframe['Number of Officers'] <- NA
    dataframe['Binning Number of Officers'] <- NA
    dataframe$`Number of Officers` <- str_count(dataframecol, coll(separator))+1
    dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="1"]<- "0"
    dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="2"]<- "1"
    dataframe$`Binning Number of Officers`[dataframe$`Number of Officers` > "2"]<- "2"
    return(dataframe)
    
}

UOF_Officers <- AllMetadata_UOF_NA
UOF_Officers <- OfficerBinning(UOF_Officers, UOF_Officers$`Officer Gender`, "|")


#binning force type according to paper notes - this needs to go 
UOF_All_FixLevels <- UOF_Officers
UOF_All_FixLevels <- UOF_All_FixLevels %>%
    mutate(`Force Type` = case_when(
    str_detect(`Force Type`, "Firearm") ~ "3",
    str_detect(`Force Type`, "Rifle") ~ "3",
    str_detect(`Force Type`, "Vehicle as Weapon") ~ "3",
    str_detect(`Force Type`, "Shotgun") ~ "3",
    str_detect(`Force Type`, "Escort Tech") ~ "2",
    str_detect(`Force Type`, "CEW") ~ "2",
    str_detect(`Force Type`, "Canine") ~ "2",
    str_detect(`Force Type`, "Baton") ~ "2",
    str_detect(`Force Type`, "NonTrad Impact Weapon") ~ "2",
    str_detect(`Force Type`, "Rifle") ~ "1",
    str_detect(`Force Type`, "Hands") ~ "1",
    str_detect(`Force Type`, "Take Down") ~ "1",
    str_detect(`Force Type`, "Takedown") ~ "1",
    str_detect(`Force Type`, "Head Strike") ~ "1",
    str_detect(`Force Type`, "Force") ~ "1",
    str_detect(`Force Type`, "Handcuffed Subject") ~ "1",
    TRUE ~ `Force Type`
    ))

write.csv(UOF_All_FixLevels,here("clean data","New Orleans","New Orleans UOF.csv"),row.names = FALSE)
