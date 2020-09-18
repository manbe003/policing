# Dallas data cleaning script
library(dplyr)
library(tidyr)
library(stringr)
setwd("~/Desktop/policing/dirty data/Dallas")
DallasShootings <- read.csv("DallasPoliceShootings.csv", stringsAsFactors = FALSE)
View(DallasShootings)

# I want to rename the columns
colnames(DallasShootings) <- c("case", "date", "location", "result_of_shot_in_subject", "subject_weapon", "subject_info", "officer_info", "grand_jury_disposition", "attorney_general_forms", "summary_url", "geolocation")

# individually changing the messed up values, Juinors, and unknowns. I dont know if there is a better way to do this
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown L/M", "Unknown, Unknown L/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown B/M", "Unknown, Unknown B/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown", "Unknown, Unknown Unknown/Unknown")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "See Summary", "Unknown, Unknown Unknown/Unknown")
DallasShootings[123, 6] = "Fuller, Antwuanne B/M"
DallasShootings[84, 6] = "West, Samuel B/M"
DallasShootings[126, 6] = "Folmar, Alton-Anthony W/M"
DallasShootings[199, 6] = "Anderson, Edward B/M"
DallasShootings[251, 6] = "Rodriguez, Robert L/M"
DallasShootings[57, 6] = "Unknown, Unknown L/M"
DallasShootings[47, 6] = "Cupples, Jimmy W/M"
DallasShootings[72, 6] = "Ross, Daniel B/M"
DallasShootings[133, 6] = "Pinedo, Gerardo L/M"
DallasShootings[228, 6] = "Paredez, Domingo L/M"
DallasShootings[18, 6] = "Luster, Desmond-Dwayne B/M"
DallasShootings[84, 7] = "Carballo, Rolando L/M; Alexander, Alphonse B/M"
DallasShootings[147, 7] = "Loeb, Jeffrey W/M"

DallasShootings$officer_info <- gsub("[.]", "", DallasShootings$officer_info)
DallasShootings$officer_info <- gsub("[!#$%&'()*+-./:;<=>?@[]^_`|~.]", "", DallasShootings$officer_info)
 
#better way to do the same thing
DallasShootings <-separate(DallasShootings, subject_info, c('last_name', 'first_race_gender'), sep=", ")
DallasShootings <-separate(DallasShootings, first_race_gender, c('first_name', 'race_gender'), sep=" ")
DallasShootings <-separate(DallasShootings, race_gender, c('subject_race', 'subject_gender'), sep="/")
DallasShootings$subject_name <- paste(DallasShootings$first_name, DallasShootings$last_name)
DallasShootings[ ,c("last_name", "first_name")] <- list(NULL)

#does not work yet
DallasShootings <-separate(DallasShootings, officer_info, c('o_last_name', 'o_first_race_gender'), sep=", ")
DallasShootings <-separate(DallasShootings, o_first_race_gender, c('o_first_name', 'o_race_gender'), sep=" ")
DallasShootings <-separate(DallasShootings, o_race_gender, c('officer_race', 'officer_gender'), sep="/")
DallasShootings$officer_name <- paste(DallasShootings$o_first_name, DallasShootings$o_last_name)
DallasShootings[ ,c("o_last_name", "o_first_name")] <- list(NULL)

#Replace B with Black, ect
DallasShootings$subject_race[DallasShootings$subject_race == "B"] <- "Black"
DallasShootings$subject_race[DallasShootings$subject_race == "W"] <- "White"
DallasShootings$subject_race[DallasShootings$subject_race == "L"] <- "Latinx"
DallasShootings$subject_race[DallasShootings$subject_race == "A"] <- "Asian"
DallasShootings$subject_race[DallasShootings$subject_race == "Unknown"] <- NA
DallasShootings$subject_gender[DallasShootings$subject_gender == "M"] <- "Male"
DallasShootings$subject_gender[DallasShootings$subject_gender == "M;"] <- "Male"
DallasShootings$subject_gender[DallasShootings$subject_gender == "F"] <- "Female"
DallasShootings$subject_gender[DallasShootings$subject_gender == "F;"] <- "Female;"
DallasShootings$subject_gender[DallasShootings$subject_gender == "Unknown"] <- NA
DallasShootings$subject_name[DallasShootings$subject_name == "Unknown Unknown"] <- NA
DallasShootings$grand_jury_disposition[DallasShootings$grand_jury_disposition == "N/A"] <- NA
DallasShootings$attorney_general_forms[DallasShootings$attorney_general_forms == "N/A"] <- NA
DallasShootings$officer_race[DallasShootings$officer_race == "B"] <- "Black"
DallasShootings$officer_race[DallasShootings$officer_race == "W"] <- "White"
DallasShootings$officer_race[DallasShootings$officer_race == "L"] <- "Latinx"
DallasShootings$officer_race[DallasShootings$officer_race == "A"] <- "Asian"
DallasShootings$officer_race[DallasShootings$officer_race == "Unknown"] <- NA
DallasShootings$officer_gender[DallasShootings$officer_gender == "M"] <- "Male"
DallasShootings$officer_gender[DallasShootings$officer_gender == "M;"] <- "Male"
DallasShootings$officer_gender[DallasShootings$officer_gender == "F"] <- "Female"
DallasShootings$officer_gender[DallasShootings$officer_gender == "F;"] <- "Female"
DallasShootings$officer_gender[DallasShootings$officer_gender == "Unknown"] <- NA
DallasShootings$officer_name[DallasShootings$officer_name == "Unknown Unknown"] <- NA


#export into clean data folder!
dir.create("~/Desktop/policing/clean data/Dallas")
write.csv(DallasShootings,"~/Desktop/policing/clean data/Dallas\\Shootings_Dallas.csv",row.names = FALSE)


#lets clean the other datasets. First we read in all of the R2R ones
Dallas_R2R_2013 <- read.csv("Police_Response_to_Resistance_-_2013.csv", stringsAsFactors = FALSE)
Dallas_R2R_2014 <- read.csv("Police__2014_Response_to_Resistance.csv", stringsAsFactors = FALSE)
Dallas_R2R_2015 <- read.csv("Police__2015_Response_to_Resistance.csv", stringsAsFactors = FALSE)
Dallas_R2R_2016 <- read.csv("Police_Response_to_Resistance_-_2016.csv", stringsAsFactors = FALSE)
Dallas_R2R_2017 <- read.csv("Police_Response_to_Resistance___2017.csv", stringsAsFactors = FALSE)
Dallas_R2R_2018 <- read.csv("Police_Response_to_Resistance_-_2018.csv", stringsAsFactors = FALSE)
Dallas_R2R_2019 <- read.csv("Police_Response_to_Resistance___2019.csv", stringsAsFactors = FALSE)


#making all of the datasets have the same column names, in the same order

Y_Dallas_R2R_2013<-cbind.data.frame(Dallas_R2R_2013[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2014<-cbind.data.frame(Dallas_R2R_2014[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2015<-cbind.data.frame(Dallas_R2R_2015[, c("OCCURRED_DT", "CURRENT_BADGE_NO", "OffSex", "OffRace", "HIRE_DT", "OffCondType", "OFF_INJURED", "OFF_HOSPITAL", "SERVICE_TYPE", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURED", "CitCondType", "CIT_ARRESTED", "CIT_INFL_ASSMT", "CitChargeType")], stringsAsFactors=FALSE)
colnames(Y_Dallas_R2R_2015) <- c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")
Y_Dallas_R2R_2016<-cbind.data.frame(Dallas_R2R_2016[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2017<-cbind.data.frame(Dallas_R2R_2017[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2018<-cbind.data.frame(Dallas_R2R_2018[, c("OCCURRED_D", "CURRENT_BAxx", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")], stringsAsFactors=FALSE)
colnames(Y_Dallas_R2R_2018) <- c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")
Y_Dallas_R2R_2019<-cbind.data.frame(Dallas_R2R_2019[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT")], stringsAsFactors=FALSE)

#binding them together into one dataset.
Dallas_R2R<- dplyr::bind_rows(Y_Dallas_R2R_2013, Y_Dallas_R2R_2014, Y_Dallas_R2R_2015, Y_Dallas_R2R_2016, Y_Dallas_R2R_2017, Y_Dallas_R2R_2018, Y_Dallas_R2R_2019)
Dallas_R2R[ ,"CURRENT_BAxx"] <- list(NULL)

#changing the column names
colnames(Dallas_R2R) <- c("date", "officer_badge_number", "officer_gender", "officer_race", "hire_date", "officer_injury_type", "officer_injured", "officer_hospital", "service_type", "force_type", "force_reason", "subject_race", "subject-gender", "subject_injury", "subject_injury_type", "subject_arrested", "subject_influence_assesment", "subject_charge")

#fixing small issues
Dallas_R2R[Dallas_R2R == "NULL"] <- NA
Dallas_R2R$subject_injury_type[Dallas_R2R$subject_injury_type == "No injuries noted or visible"] <- NA
Dallas_R2R$officer_injury_type[Dallas_R2R$officer_injury_type == "No injuries noted or visible"] <- NA
Dallas_R2R$subject_injury[Dallas_R2R$subject_injury == "false"] <- "no"
Dallas_R2R$subject_injury[Dallas_R2R$subject_injury == "true"] <- "yes"
Dallas_R2R$subject_arrested[Dallas_R2R$subject_arrested == "false"] <- "no"
Dallas_R2R$subject_arrested[Dallas_R2R$subject_arrested == "true"] <- "yes"
Dallas_R2R$officer_injury[Dallas_R2R$officer_injury == "false"] <- "no"
Dallas_R2R$officer_injury[Dallas_R2R$officert_injury == "true"] <- "yes"
Dallas_R2R$officer_hospital[Dallas_R2R$officer_hospital == "false"] <- "no"
Dallas_R2R$officer_hospital[Dallas_R2R$officert_hospital == "true"] <- "yes"
Dallas_R2R$subject_influence_assesment[Dallas_R2R$subject_influence_assesment == "Unknown"] <- NA

#fix this


#fix date and hire date
stopwords = c(" 12:00:00 AM") 
Dallas_R2R$hire_date <- gsub(paste0(stopwords, collapse = "|"),"", Dallas_R2R$hire_date)
Dallas_R2R$hire_date <- gsub(paste0(stopwords, collapse = "|"),"", Dallas_R2R$hire_date)

dir.create("~/Desktop/policing/clean data/Dallas")
write.csv(Dallas_R2R,"~/Desktop/policing/clean data/Dallas\\Dallas_R2R.csv",row.names = FALSE)


View(Dallas_R2R)



