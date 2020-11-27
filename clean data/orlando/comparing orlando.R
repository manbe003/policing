#####################

#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)
library(here)

#turn off scientific notation
options(scipen=999)

# change these two lines!
#I want to call in my datasets (use of force, shootings).
library(readr)
UOF <- read.csv(file=here('dirty data/orlando/OPD_Response_To_Resistance.csv'), stringsAsFactors = FALSE)
shootings <- read.csv(file=here('dirty data/orlando/OPD_Officer-Involved_Shootings.csv'), stringsAsFactors = FALSE)

#making blank values in Incident date
UOF_NA<-UOF
UOF_NA$Incident.Date.Time[UOF_NA$Incident.Date.Time==""]<-NA

#numericize the data (UOF)
#first split year/date into two separate variables
SplitDateTime_UOF<-strsplit(as.character(UOF_NA$Incident.Date.Time),"\\s")
SplitDateTime_UOF<-do.call(rbind, SplitDateTime_UOF)
colnames(SplitDateTime_UOF)<-(c("date","time", "hour"))
SplitDateTime_UOF<-as.data.frame(SplitDateTime_UOF, stringsAsFactors=FALSE)

##K: not sure exactly what this is doing
NoTitle_UOF <- UOF_NA[UOF_NA$Incident.Date.Time != "", ]

##seperating officer race into different officers
shootings <-separate(shootings, Officer.Race, c("Officer One Race", "Officer Two Race", "Officer Three Race", "Officer Four Race" ), sep=", ")


#making a table with all relevant metadata for UOF
AllMetadata_UOF<-cbind.data.frame(NoTitle_UOF$ï..Incident.Number,SplitDateTime_UOF$date,SplitDateTime_UOF$time,SplitDateTime_UOF$hour,NoTitle_UOF$Officers.Race,NoTitle_UOF$Officers.Ethnicity,NoTitle_UOF$Officers.Sex,NoTitle_UOF$Offenders.Race,NoTitle_UOF$Offenders.Ethnicity,NoTitle_UOF$Offenders.Sex, stringsAsFactors=FALSE)

colnames(AllMetadata_UOF)<-(c("Incident Number","date","time", "hour", "Officers.Race","Officers.Ethnicity","Officers.Sex", "Offenders.Race","Offenders.Ethnicity","Offenders.Sex"))

#make all null values = U for UOF
AllMetadata_UOF_NA<-AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF=="Not Specified"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="-"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="X"]<-NA

#Change race, ethnicity, sex to be consistent with UOF
AllMetadata_UOF_Fix<-AllMetadata_UOF_NA

AllMetadata_UOF_Fix$Officers.Race <- gsub('W', 'White', AllMetadata_UOF_Fix$Officers.Race)
AllMetadata_UOF_Fix$Officers.Race <- gsub('A', 'Asian', AllMetadata_UOF_Fix$Officers.Race)
AllMetadata_UOF_Fix$Officers.Race <- gsub('B', 'Black', AllMetadata_UOF_Fix$Officers.Race)
AllMetadata_UOF_Fix$Officers.Race <- gsub('U', NA, AllMetadata_UOF_Fix$Officers.Race)

AllMetadata_UOF_Fix$Officers.Ethnicity <- gsub('NH', 'Non-hispanic', AllMetadata_UOF_Fix$Officers.Ethnicity)
AllMetadata_UOF_Fix$Officers.Ethnicity <- gsub('HI', 'Hispanic', AllMetadata_UOF_Fix$Officers.Ethnicity)
AllMetadata_UOF_Fix$Officers.Ethnicity <- gsub('U', NA, AllMetadata_UOF_Fix$Officers.Ethnicity)

AllMetadata_UOF_Fix$Officers.Sex <- gsub('M', 'Male', AllMetadata_UOF_Fix$Officers.Sex)
AllMetadata_UOF_Fix$Officers.Sex <- gsub('F', 'Female', AllMetadata_UOF_Fix$Officers.Sex)
AllMetadata_UOF_Fix$Officers.Sex <- gsub('U', NA, AllMetadata_UOF_Fix$Officers.Sex)

AllMetadata_UOF_Fix$Offenders.Race <- gsub('W', 'White', AllMetadata_UOF_Fix$Offenders.Race)
AllMetadata_UOF_Fix$Offenders.Race <- gsub('A', 'Asian', AllMetadata_UOF_Fix$Offenders.Race)
AllMetadata_UOF_Fix$Offenders.Race <- gsub('B', 'Black', AllMetadata_UOF_Fix$Offenders.Race)
AllMetadata_UOF_Fix$Offenders.Race <- gsub('U', NA, AllMetadata_UOF_Fix$Offenders.Race)

AllMetadata_UOF_Fix$Offenders.Ethnicity <- gsub('NH', 'Non-hispanic', AllMetadata_UOF_Fix$Offenders.Ethnicity)
AllMetadata_UOF_Fix$Offenders.Ethnicity <- gsub('HI', 'Hispanic', AllMetadata_UOF_Fix$Offenders.Ethnicity)
AllMetadata_UOF_Fix$Offenders.Ethnicity <- gsub('U', NA, AllMetadata_UOF_Fix$Offenders.Ethnicity)

AllMetadata_UOF_Fix$Offenders.Sex <- gsub('M', 'Male', AllMetadata_UOF_Fix$Offenders.Sex)
AllMetadata_UOF_Fix$Offenders.Sex <- gsub('F', 'Female', AllMetadata_UOF_Fix$Offenders.Sex)
AllMetadata_UOF_Fix$Offenders.Sex <- gsub('U', NA, AllMetadata_UOF_Fix$Offenders.Sex)

#####################

#numericize the data (SHOOTINGS)
NoTitle_shootings <- shootings[shootings$Date != "", ]

#making a table with all relevant metadata for shootings
AllMetadata_shootings<-cbind.data.frame(NoTitle_shootings$ï..Case..,NoTitle_shootings$Date,NoTitle_shootings$`Officer One Race`,NoTitle_shootings$`Officer Two Race`,NoTitle_shootings$`Officer Three Race`,NoTitle_shootings$`Officer Four Race`,NoTitle_shootings$Ethnicity,NoTitle_shootings$Officer.Gender,NoTitle_shootings$Suspect.Race,NoTitle_shootings$Suspect.Gender,NoTitle_shootings$Suspect.Hit,NoTitle_shootings$Fatal, stringsAsFactors=FALSE)
colnames(AllMetadata_shootings)<-(c("Incident Number","date","Officer One Race","Officer Two Race","Officer Three Race","Officer Four Race","Officer Ethnicity","Officer.Gender", "Suspect.Race","Suspect.Gender","Suspect.Hit","Fatal"))

#make all null values NA for shootings
AllMetadata_shootings_NA<-AllMetadata_shootings
AllMetadata_shootings_NA[AllMetadata_shootings=="n/a"]<-NA
AllMetadata_shootings_NA[AllMetadata_shootings=="Exempt"]<-NA
AllMetadata_shootings_NA[AllMetadata_shootings=="Undetermined"]<-NA
AllMetadata_shootings_NA[AllMetadata_shootings=="Omitted"]<-NA

#Change race, ethnicity, sex to be consisted with shootings
AllMetadata_shootings_Fix<-AllMetadata_shootings_NA

AllMetadata_shootings_Fix$`Officer One Race` <- gsub('W', 'White', AllMetadata_shootings_Fix$`Officer One Race`)
AllMetadata_shootings_Fix$`Officer One Race`<- gsub('A', 'Asian', AllMetadata_shootings_Fix$`Officer One Race`)
AllMetadata_shootings_Fix$`Officer One Race`<- gsub('B', 'Black', AllMetadata_shootings_Fix$`Officer One Race`)
AllMetadata_shootings_Fix$`Officer One Race`<- gsub('U', NA, AllMetadata_shootings_Fix$`Officer One Race`)
AllMetadata_shootings_Fix$`Officer One Race`<- gsub('O', NA, AllMetadata_shootings_Fix$`Officer One Race`)

AllMetadata_shootings_Fix$`Officer Two Race` <- gsub('W', 'White', AllMetadata_shootings_Fix$`Officer Two Race`)
AllMetadata_shootings_Fix$`Officer Two Race`<- gsub('A', 'Asian', AllMetadata_shootings_Fix$`Officer Two Race`)
AllMetadata_shootings_Fix$`Officer Two Race`<- gsub('B', 'Black', AllMetadata_shootings_Fix$`Officer Two Race`)
AllMetadata_shootings_Fix$`Officer Two Race`<- gsub('U', NA, AllMetadata_shootings_Fix$`Officer Two Race`)
AllMetadata_shootings_Fix$`Officer Two Race`<- gsub('O', NA, AllMetadata_shootings_Fix$`Officer Two Race`)

AllMetadata_shootings_Fix$`Officer Three Race` <- gsub('W', 'White', AllMetadata_shootings_Fix$`Officer Three Race`)
AllMetadata_shootings_Fix$`Officer Three Race`<- gsub('A', 'Asian', AllMetadata_shootings_Fix$`Officer Three Race`)
AllMetadata_shootings_Fix$`Officer Three Race`<- gsub('B', 'Black', AllMetadata_shootings_Fix$`Officer Three Race`)
AllMetadata_shootings_Fix$`Officer Three Race`<- gsub('U', NA, AllMetadata_shootings_Fix$`Officer Three Race`)
AllMetadata_shootings_Fix$`Officer Three Race`<- gsub('O', NA, AllMetadata_shootings_Fix$`Officer Three Race`)

AllMetadata_shootings_Fix$`Officer Four Race` <- gsub('W', 'White', AllMetadata_shootings_Fix$`Officer Four Race`)
AllMetadata_shootings_Fix$`Officer Four Race`<- gsub('A', 'Asian', AllMetadata_shootings_Fix$`Officer Four Race`)
AllMetadata_shootings_Fix$`Officer Four Race`<- gsub('B', 'Black', AllMetadata_shootings_Fix$`Officer Four Race`)
AllMetadata_shootings_Fix$`Officer Four Race`<- gsub('U', NA, AllMetadata_shootings_Fix$`Officer Four Race`)
AllMetadata_shootings_Fix$`Officer Four Race`<- gsub('O', NA, AllMetadata_shootings_Fix$`Officer Four Race`)

AllMetadata_shootings_Fix$`Officer Ethnicity`<- gsub('N', 'Non-hispanic', AllMetadata_shootings_Fix$`Officer Ethnicity`)
AllMetadata_shootings_Fix$`Officer Ethnicity`<- gsub('H', 'Hispanic', AllMetadata_shootings_Fix$`Officer Ethnicity`)
AllMetadata_shootings_Fix$`Officer Ethnicity`<- gsub('U', NA, AllMetadata_shootings_Fix$`Officer Ethnicity`)
AllMetadata_shootings_Fix$`Officer Ethnicity`<- gsub('W', 'White', AllMetadata_shootings_Fix$`Officer Ethnicity`)
AllMetadata_shootings_Fix$`Officer Ethnicity`<- gsub('A', 'Asian', AllMetadata_shootings_Fix$`Officer Ethnicity`)

AllMetadata_shootings_Fix$Officer.Gender <- gsub('M', 'Male', AllMetadata_shootings_Fix$Officer.Gender)
AllMetadata_shootings_Fix$Officer.Gender <- gsub('F', 'Female', AllMetadata_shootings_Fix$Officer.Gender)
AllMetadata_shootings_Fix$Officer.Gender <- gsub('U', NA, AllMetadata_shootings_Fix$Officer.Gender)

AllMetadata_shootings_Fix$Suspect.Gender <- gsub('F', 'Female', AllMetadata_shootings_Fix$Suspect.Gender)
AllMetadata_shootings_Fix$Suspect.Gender <- gsub('M', 'Male', AllMetadata_shootings_Fix$Suspect.Gender)

#save as data file/set
write.csv(AllMetadata_shootings_Fix,(file=here('clean data/orlando/shooting (cleaned).csv')), row.names = FALSE)
write.csv(AllMetadata_UOF_Fix,(File=here('clean data/orlando/UOF (cleaned).csv')), row.names = FALSE)
