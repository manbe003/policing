# Dallas data cleaning script
library(dplyr)
library(tidyr)
library(stringr)
library(here)



setwd(here("dirty data","Dallas"))
DallasShootings <- read.csv("DallasPoliceShootings.csv", stringsAsFactors = FALSE)

# I want to rename the columns
colnames(DallasShootings) <- c("case", "date", "location", "result_of_shot_in_subject", "subject_weapon", "subject_info", "officer_info", "grand_jury_disposition", "attorney_general_forms", "summary_url", "geolocation")

# individually changing the messed up values, Juniors, and unknowns. 
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown L/M", "Unknown, Unknown L/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown B/M", "Unknown, Unknown B/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown", "Unknown, Unknown Unknown/Unknown")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "See Summary", "Unknown, Unknown Unknown/Unknown")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "West, Samuel III B/M", "West, Samuel B/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Unknown L/M; Unknown L/F", "Unknown, Unknown L/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Luster, Desmond Dwayne B/M", "Luster, Desmond-Dwayne B/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Folmar Jr., Alton Anthony W/M", "Folmar, Alton-Anthony W/M")
DallasShootings$subject_info <- replace(as.character(DallasShootings$subject_info), DallasShootings$subject_info == "Dontrell Terrell B/M", "Dontrell, Terrell B/M")

DallasShootings$officer_info <- gsub("Jr. ", "", DallasShootings$officer_info)
DallasShootings$subject_info <- gsub("Jr. ", "", DallasShootings$subject_info)

#changing values that had special characters
DallasShootings[123, 6] = "Fuller, Antwuanne B/M"
DallasShootings[147, 7] = "Loeb, Jeffrey W/M"

View(DallasShootings)

DallasShootings$officer_info[DallasShootings$officer_info == "Strand, Emmanuel A/M, Guerra, Carlos L/M"] <- "Strand, Emmanuel A/M; Guerra, Carlos L/M"

DallasShootings = separate_rows(DallasShootings,"officer_info",sep = "; ")

#seperating subject info and officer info
DallasShootings <-separate(DallasShootings, subject_info, c('last_name', 'first_race_gender'), sep=", ")
DallasShootings <-separate(DallasShootings, first_race_gender, c('first_name', 'race_gender'), sep=" ")
DallasShootings <-separate(DallasShootings, race_gender, c('subject_race', 'subject_gender'), sep="/")
DallasShootings$subject_name <- paste(DallasShootings$first_name, DallasShootings$last_name)
DallasShootings[ ,c("last_name", "first_name")] <- list(NULL)

DallasShootings <-separate(DallasShootings, officer_info, c('o_last_name', 'o_first_race_gender'), sep=", ")
DallasShootings <-separate(DallasShootings, o_first_race_gender, c('o_first_name', 'o_race_gender'), sep=" ")
DallasShootings <-separate(DallasShootings, o_race_gender, c('officer_race', 'officer_gender'), sep="/")
DallasShootings$officer_name <- paste(DallasShootings$o_first_name, DallasShootings$o_last_name)
DallasShootings[ ,c("o_last_name", "o_first_name")] <- list(NULL)

#Replace B with Black, W with White, Unknown with NA,blank values with NA, ect
DallasShootings$subject_race[DallasShootings$subject_race == "B"] <- "Black"
DallasShootings$subject_race[DallasShootings$subject_race == "W"] <- "White"
DallasShootings$subject_race[DallasShootings$subject_race == "L"] <- "Latinx"
DallasShootings$subject_race[DallasShootings$subject_race == "A"] <- "Asian"
DallasShootings$subject_race[DallasShootings$subject_race == "Unknown"] <- NA
DallasShootings$subject_gender[DallasShootings$subject_gender == "M"] <- "Male"
DallasShootings$subject_gender[DallasShootings$subject_gender == "M;"] <- "Male"
DallasShootings$subject_gender[DallasShootings$subject_gender == "F"] <- "Female"
DallasShootings$subject_gender[DallasShootings$subject_gender == "F;"] <- "Female"
DallasShootings$subject_gender[DallasShootings$subject_gender == "Unknown"] <- NA
DallasShootings$subject_name[DallasShootings$subject_name == "Unknown Unknown"] <- NA
DallasShootings[DallasShootings == "N/A"] <- NA
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
DallasShootings$subject_race[DallasShootings$subject_race == ""] <- NA
DallasShootings$officer_race[DallasShootings$officer_race == ""] <- NA

#cleaning weapons
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Toy Handun"] <- "Toy Handgun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "BB Gun"] <- "Toy Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "BB Rifle"] <- "Toy Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Toy Handgun"] <- "Toy Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Toy Rifle"] <- "Toy Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Paint Ball Rifle"] <- "Toy Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "TASER"] <- "Taser"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Rock"] <- "Questionable Weapon"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Assault Rifle"] <- "Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Unknown Firearm"] <- "Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Handgun"] <- "Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Shotgun"] <- "Gun"
DallasShootings$subject_weapon[DallasShootings$subject_weapon == "Rifle"] <- "Gun"


#lets clean the the response to resistance dataset. We have to read in all of the R2R ones by year.
Dallas_R2R_2013 <- read.csv("Police_Response_to_Resistance_-_2013.csv", stringsAsFactors = FALSE)
Dallas_R2R_2014 <- read.csv("Police__2014_Response_to_Resistance.csv", stringsAsFactors = FALSE)
Dallas_R2R_2015 <- read.csv("Police__2015_Response_to_Resistance.csv", stringsAsFactors = FALSE)
Dallas_R2R_2016 <- read.csv("Police_Response_to_Resistance_-_2016.csv", stringsAsFactors = FALSE)
Dallas_R2R_2017 <- read.csv("Police_Response_to_Resistance___2017.csv", stringsAsFactors = FALSE)
Dallas_R2R_2018 <- read.csv("Police_Response_to_Resistance_-_2018.csv", stringsAsFactors = FALSE)
Dallas_R2R_2019 <- read.csv("Police_Response_to_Resistance___2019.csv", stringsAsFactors = FALSE)
View(Dallas_R2R_2018)

#making all of the datasets have the same column names, in the same order --- "RA", "BEAT", "SECTOR", "DIVISION", "DIST_NAME"

Y_Dallas_R2R_2013<-cbind.data.frame(Dallas_R2R_2013[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2014<-cbind.data.frame(Dallas_R2R_2014[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2015<-cbind.data.frame(Dallas_R2R_2015[, c("OCCURRED_DT", "CURRENT_BADGE_NO", "OffSex", "OffRace", "HIRE_DT", "OffCondType", "OFF_INJURED", "OFF_HOSPITAL", "SERVICE_TYPE", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURED", "CitCondType", "CIT_ARRESTED", "CIT_INFL_ASSMT", "CitChargeType", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)
colnames(Y_Dallas_R2R_2015) <- c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")
Y_Dallas_R2R_2016<-cbind.data.frame(Dallas_R2R_2016[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2017<-cbind.data.frame(Dallas_R2R_2017[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)
Y_Dallas_R2R_2018<-cbind.data.frame(Dallas_R2R_2018[, c("OCCURRED_D", "CURRENT_BAxx", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)
colnames(Y_Dallas_R2R_2018) <- c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")
Y_Dallas_R2R_2019<-cbind.data.frame(Dallas_R2R_2019[, c("OCCURRED_D", "CURRENT_BA", "OffSex", "OffRace", "HIRE_DT", "OffCondTyp", "OFF_INJURE", "OFF_HOSPIT", "SERVICE_TY", "ForceType", "UOF_REASON", "CitRace", "CitSex", "CIT_INJURE", "CitCondTyp", "CIT_ARREST", "CIT_INFL_A", "CitChargeT", "RA", "BEAT", "SECTOR", "DIVISION")], stringsAsFactors=FALSE)

Y_Dallas_R2R_2018$BEAT <- as.numeric(Y_Dallas_R2R_2018$BEAT)
Y_Dallas_R2R_2018$RA <- as.numeric(Y_Dallas_R2R_2018$RA)
Y_Dallas_R2R_2018$SECTOR <- as.numeric(Y_Dallas_R2R_2018$SECTOR)

#binding them together into one dataset.
Dallas_R2R<- dplyr::bind_rows(Y_Dallas_R2R_2013, Y_Dallas_R2R_2014, Y_Dallas_R2R_2015, Y_Dallas_R2R_2016, Y_Dallas_R2R_2017, Y_Dallas_R2R_2018, Y_Dallas_R2R_2019)
Dallas_R2R[ ,"CURRENT_BAxx"] <- list(NULL)

#changing the column names
colnames(Dallas_R2R) <- c("date", "officer_badge_number", "officer_gender", "officer_race", "hire_date", "officer_injury_type", "officer_injury", "officer_hospital", "service_type", "force_type", "force_reason", "subject_race", "subject_gender", "subject_injury", "subject_injury_type", "subject_arrested", "subject_influence_assesment", "subject_charge", "reporting_area", "beat", "sector", "division")

#fixing small issues
Dallas_R2R[Dallas_R2R == "NULL"] <- NA
Dallas_R2R[Dallas_R2R == "No injuries noted or visible"] <- NA
Dallas_R2R[Dallas_R2R == "Yes"] <- TRUE
Dallas_R2R$subject_injury[Dallas_R2R$subject_injury == "No"] <- FALSE
Dallas_R2R$subject_arrested[Dallas_R2R$subject_arrested == "No"] <- FALSE
Dallas_R2R$officer_injury[Dallas_R2R$officer_injury == "No"] <- FALSE
Dallas_R2R$officer_hospital[Dallas_R2R$officer_hospital == "No"] <- FALSE
Dallas_R2R[Dallas_R2R == "false"] <- FALSE
Dallas_R2R[Dallas_R2R == "true"] <- TRUE

Dallas_R2R <- Dallas_R2R %>% mutate_all(na_if,"")
DallasShootings <- DallasShootings %>% mutate_all(na_if,"Unknown")
DallasShootings <- DallasShootings %>% mutate_all(na_if,"")
Dallas_R2R <- Dallas_R2R %>% mutate_all(na_if,"Unknown")


Dallas_R2R$subject_race[Dallas_R2R$subject_race == "Other"] <- NA
Dallas_R2R[Dallas_R2R == "American Ind"] <- "Indigenous"

#fix date and hire date
stopwords = c(" 12:00:00 AM")
Dallas_R2R$hire_date <- gsub(paste0(stopwords, collapse = "|"),"", Dallas_R2R$hire_date)
Dallas_R2R$date <- gsub(paste0(stopwords, collapse = "|"),"", Dallas_R2R$date)

DallasLinker <- read.csv("DallasLink.csv", stringsAsFactors = FALSE)

Dallas_R2R_linked <- merge(DallasLinker, Dallas_R2R, by = "officer_badge_number", all.y = TRUE)
Dallas_shootings_linked <- merge(DallasLinker, DallasShootings, by = "officer_name", all.y = TRUE)

View(Dallas_R2R_linked)

Dallas_shootings_unique<-subset(Dallas_shootings_linked, !duplicated(subject_name))

#export clean datasets into clean data folder
write.csv(Dallas_shootings_unique,here("clean data","Dallas","Dallas_shootings_unique_testing.csv"),row.names = FALSE)
write.csv(Dallas_shootings_linked,here("clean data","Dallas","Dallas_shootings_testing.csv"),row.names = FALSE)
write.csv(Dallas_R2R_linked,here("clean data","Dallas","Dallas_R2R_linked_testing.csv"),row.names = FALSE)

View(Dallas_shootings_linked)
