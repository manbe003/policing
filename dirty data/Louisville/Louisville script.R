#first I want to call libraries 
library (dplyr)
library (tidyr)
library(stringr)
library(here)
install.packages("Here")

#set working directory
setwd("~/Desktop/policing/dirty data/Louisville")

#I want to call in my datasets (citations, stops, shootings)
LouisvilleStops<-read.csv(file = here('dirty data/Louisville/Louisville stops.csv'), stringsAsFactors = FALSE)
LouisvilleShootings<-read.csv(file = here('dirty data/Louisville/all_louisville_OIS.csv'), stringsAsFactors = FALSE)
LouisvilleCitations <- read.csv(unz("Louisville_citations.csv.zip", "Louisville_citations.csv"), stringsAsFactors = FALSE)

#Below is my work making citations into a usable size by deleting columns.
#LouisvilleCitations <- read.csv(unz("UniformCitationData .csv.zip", "UniformCitationData .csv"))
#LouisvilleCitations[ ,c("AGENCY_DESC", "ID", "CITATION_YEAR", "ASCF_CODE", "STATUTE", "UCR_CODE", "PERSONS_HOME_CITY", "PERSONS_HOME_STATE", "PERSONS_HOME_ZIP")] <- list(NULL)
#write.csv(LouisvilleCitations,"~/Desktop/policing/dirty data/Louisville/Louisville_citations.csv",row.names = FALSE)

#Clean Shootings
#delete ethnicity columns, as it is all none, N, or U. Delete last row, as there is nothing in it.
LouisvilleShootings[,c("subject_ethnicity", "officer_ethnicity")] <- list(NULL)
LouisvilleShootings<-LouisvilleShootings[1:100,]

#Replace B with Black, W with White, Unknown with NA,blank values with NA, ect. Get rid of titles in fromt of officer names.
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "B"] <- "Black"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "W"] <- "White"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "L"] <- "Latinx"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "M"] <- "Middle Eastern"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "H"] <- "Hispanic"
LouisvilleShootings$subject_sex[LouisvilleShootings$subject_sex == "M"] <- "Male"
LouisvilleShootings$subject_sex[LouisvilleShootings$subject_sex == "F"] <- "Female"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "B"] <- "Black"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "W"] <- "White"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "W                "] <- "White"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "A"] <- "Asian"
LouisvilleShootings$officer_sex[LouisvilleShootings$officer_sex == "M"] <- "Male"
LouisvilleShootings$officer_sex[LouisvilleShootings$officer_sex == "F"] <- "Female"
LouisvilleShootings$lethal[LouisvilleShootings$lethal == "Y"] <- "Yes"
LouisvilleShootings$lethal[LouisvilleShootings$lethal == "N"] <- "No"
LouisvilleShootings$lethal[LouisvilleShootings$lethal == "YS"] <- "Self-inflicted"
LouisvilleShootings$officer_name <- gsub("Ofc. ", "", LouisvilleShootings$officer_name)
LouisvilleShootings$officer_name <- gsub("Det. ", "", LouisvilleShootings$officer_name)
LouisvilleShootings$officer_name <- gsub("Lt. ", "", LouisvilleShootings$officer_name)
LouisvilleShootings$officer_name <- gsub("Sgt. ", "", LouisvilleShootings$officer_name)
LouisvilleShootings$officer_name <- gsub("Qfc. ", "", LouisvilleShootings$officer_name)
#Fixing years of service.
LouisvilleShootings$years_of_service <- gsub("years", "", LouisvilleShootings$years_of_service )
LouisvilleShootings$years_of_service[LouisvilleShootings$years_of_service == "8yrs. 4m"] <- "8"
LouisvilleShootings$years_of_service[LouisvilleShootings$years_of_service == "Varies"] <- NA
#fixing capitalization
LouisvilleShootings$case_status <- tolower(LouisvilleShootings$case_status)
LouisvilleShootings$case_status <- str_to_title(LouisvilleShootings$case_status)
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "SHOOTING INVESTIGATION - LMPD INVOLVED"] <- "Shooting investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "DEATH INVESTIGATION - LMPD INVOLVED"] <- "Death investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "SHOOTING INVESTIGATION- LMPD INVOLVED"] <- "Shooting investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "DEATH INVESTIGATION- LMPD INVOLVED"] <- "Death investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "DEATH INVESTIGATION-LMPD INVOLVED"] <- "Death investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "SHOOTING INVESTIGATION-LMPD INVOLVED"] <- "Shooting investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "SHOOTING INVESTIGATIONS - LMPD INVOLVED"] <- "Shooting investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "OIS"] <- "Officer involved shooting"
#Making weapons less specific
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Shotgun"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Handgun"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Rifle"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "BB gun"] <- "Fake gun"
#fixing years of service, 20+ becomes just 20.
LouisvilleShootings$years_of_service[LouisvilleShootings$years_of_service == "20+"] <- "20"
LouisvilleShootings$years_of_service[LouisvilleShootings$years_of_service == "Varies"] <- NA
LouisvilleShootings$officer_age[LouisvilleShootings$officer_age == "Varies"] <- NA
LouisvilleShootings<-separate_rows(LouisvilleShootings,17:21, sep = "/", convert = FALSE)
#fixing irregularity in row 11. Making it UNKNOWN instead of blank. I don't know how else to do this, im sure there is a better way. 
LouisvilleShootings[11, 15] = "UNKNOWN"

#Making a function that fills the blank values with the one above (thanks, Stack Overflow!)
fillTheBlanks <- function(x, missing=""){
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
}
#Using this function on all of the necessary columns.
LouisvilleShootings$month <- fillTheBlanks(LouisvilleShootings$month)
LouisvilleShootings$PIU_number <- fillTheBlanks(LouisvilleShootings$PIU_number)
LouisvilleShootings$incident_number <- fillTheBlanks(LouisvilleShootings$incident_number)
LouisvilleShootings$day <- fillTheBlanks(LouisvilleShootings$day)
LouisvilleShootings$time <- fillTheBlanks(LouisvilleShootings$time)
LouisvilleShootings$division <- fillTheBlanks(LouisvilleShootings$division)
LouisvilleShootings$investigation_type <- fillTheBlanks(LouisvilleShootings$investigation_type)
LouisvilleShootings$case_status <- fillTheBlanks(LouisvilleShootings$case_status)
LouisvilleShootings$subject_name <- fillTheBlanks(LouisvilleShootings$subject_name)
LouisvilleShootings$subject_age <- fillTheBlanks((LouisvilleShootings$subject_age))
LouisvilleShootings$subject_weapon <- fillTheBlanks(LouisvilleShootings$subject_weapon)
LouisvilleShootings$officer_age <- fillTheBlanks(LouisvilleShootings$officer_age)
LouisvilleShootings$officer_name <- fillTheBlanks(LouisvilleShootings$officer_name)
LouisvilleShootings$years_of_service <- fillTheBlanks(LouisvilleShootings$years_of_service)
LouisvilleShootings$address <- fillTheBlanks(LouisvilleShootings$address)
LouisvilleShootings$lethal <- fillTheBlanks(LouisvilleShootings$lethal)
LouisvilleShootings$lethal <- fillTheBlanks(LouisvilleShootings$lethal, missing = "Continued")
LouisvilleShootings$incident_number <- fillTheBlanks(LouisvilleShootings$incident_number, missing = "Continued")

#Since they stopped inputting beat partway through, i'm changing the first one that was not input to 0 from NA. 
#Then, the fill in the blank function will make all of them zero, and I will change 0 back to NA. 
#very high chance that this is not a good solution but it works so...
LouisvilleShootings[70, 9] = 0
LouisvilleShootings$beat[is.na(LouisvilleShootings$beat)] <- ""
LouisvilleShootings$beat <- fillTheBlanks(LouisvilleShootings$beat)
LouisvilleShootings$beat[LouisvilleShootings$beat == 0] <- NA

LouisvilleShootings[LouisvilleShootings == "U"] <- NA
LouisvilleShootings[LouisvilleShootings == "UNKNOWN"] <- NA
LouisvilleShootings[LouisvilleShootings == "Unknown"] <- NA


#Cleaning citations
colnames(LouisvilleCitations)<-(c("case_number", "citation_control_number", "citation_type", "citation_date", "citation_location", "division", "beat", "subject_gender", "subject_race", "subject_ethnicity", "subject_age", "violation_code", "charge_description", "UCR_description"))
LouisvilleCitations$subject_race[LouisvilleCitations$subject_race == "B "] <- "Black"
LouisvilleCitations$subject_race[LouisvilleCitations$subject_race == "W "] <- "White"
LouisvilleCitations$subject_race[LouisvilleCitations$subject_race == "L "] <- "Latinx"
LouisvilleCitations$subject_race[LouisvilleCitations$subject_race == "H "] <- "?"
LouisvilleCitations$subject_ethnicity[LouisvilleCitations$subject_ethnicity == "N"] <- "Non-Hispanic"
LouisvilleCitations$subject_ethnicity[LouisvilleCitations$subject_ethnicity == "H"] <- "Hispanic"
LouisvilleCitations$subject_ethnicity[LouisvilleCitations$subject_ethnicity == "U"] <- NA
LouisvilleCitations$subject_gender[LouisvilleCitations$subject_gender == "M"] <- "Male"
LouisvilleCitations$subject_gender[LouisvilleCitations$subject_gender == "F"] <- "Female"
LouisvilleCitations[LouisvilleCitations == ""] <- NA
LouisvilleCitations[LouisvilleCitations == "        "] <- NA
LouisvilleCitations[LouisvilleCitations == "            "] <- NA
LouisvilleCitations[LouisvilleCitations == "  "] <- NA


#Cleaning stops
LouisvilleStops[ ,c("ID", "ACTIVITY_TIME")] <- list(NULL)
colnames(LouisvilleStops)<-(c("type_of_stop", "citation_control_number", "activity_results", "officer_gender", "officer_race", "officer_age_range", "date", "location", "division", "beat", "driver_gender", "driver_race", "driver_age_range", "number_of_passengers", "was_vehicle_searched", "reason_for_search"))
LouisvilleStops$officer_race <- tolower(LouisvilleStops$officer_race)
LouisvilleStops$officer_race <- str_to_title(LouisvilleStops$officer_race)
LouisvilleStops$driver_race <- tolower(LouisvilleStops$driver_race)
LouisvilleStops$driver_race <- str_to_title(LouisvilleStops$driver_race)
LouisvilleStops$officer_gender[LouisvilleStops$officer_gender == "F"] <- "Female"
LouisvilleStops$officer_gender[LouisvilleStops$officer_gender == "M"] <- "Male"
LouisvilleStops$driver_gender[LouisvilleStops$driver_gender == "F"] <- "Female"
LouisvilleStops$driver_gender[LouisvilleStops$driver_gender == "M"] <- "Male"
LouisvilleStops$was_vehicle_searched[LouisvilleStops$was_vehicle_searched == "NO"] <- FALSE
LouisvilleStops$was_vehicle_searched[LouisvilleStops$was_vehicle_searched == "YES"] <- TRUE
LouisvilleStops$division = substr(LouisvilleStops$division,1,nchar(LouisvilleStops$division)-11)
LouisvilleStops$beat <- gsub("BEAT ", "", LouisvilleStops$beat)
LouisvilleStops$number_of_passengers <- as.numeric(LouisvilleStops$number_of_passengers)
LouisvilleStops$divison <- as.numeric(LouisvilleStops$number_of_passengers)
LouisvilleStops[LouisvilleStops == ""] <- NA
LouisvilleStops[LouisvilleStops == " "] <- NA
LouisvilleStops$citation_control_number[LouisvilleStops$citation_control_number == 0] <- NA

#write files into clean data folder
setwd("~/Desktop/policing/clean data/Louisville")
write.csv(LouisvilleCitations,"~/Desktop/policing/clean data/Louisville/LouisvilleCitations.csv",row.names = FALSE)
write.csv(LouisvilleShootings,"~/Desktop/policing/clean data/Louisville/LouisvilleShootings.csv",row.names = FALSE)
write.csv(LouisvilleStops,"~/Desktop/policing/clean data/Louisville/LouisvilleStops.csv",row.names = FALSE)
