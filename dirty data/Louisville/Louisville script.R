#first I want to call libraries 
library (dplyr)
library (tidyr)
library(stringr)

#set working directory
setwd("~/Desktop/policing/dirty data/Louisville")

#I want to call in my datasets (citations, stops, shootings)

LouisvilleStops<-read.csv(file = 'Louisville stops.csv', stringsAsFactors = FALSE)

LouisvilleShootings<-read.csv(file = 'all_louisville_OIS.csv', stringsAsFactors = FALSE)

LouisvilleCitations <- read.csv(unz("UniformCitationData .csv.zip", "UniformCitationData .csv"))

#Clean Shootings

#delete subject ethnicity, as it is all none, N, or U
LouisvilleShootings[,c("subject_ethnicity", "officer_ethnicity")] <- list(NULL)

LouisvilleShootings[LouisvilleShootings == ""] <- NA
LouisvilleShootings[LouisvilleShootings == "U"] <- NA
LouisvilleShootings[LouisvilleShootings == "UNKNOWN"] <- NA

#Replace B with Black, W with White, Unknown with NA,blank values with NA, ect
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "B"] <- "Black"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "W"] <- "White"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "L"] <- "Latinx"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "M"] <- "?"
LouisvilleShootings$subject_race[LouisvilleShootings$subject_race == "H"] <- "?"
LouisvilleShootings$subject_sex[LouisvilleShootings$subject_sex == "M"] <- "Male"
LouisvilleShootings$subject_sex[LouisvilleShootings$subject_sex == "F"] <- "Female"

LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "B"] <- "Black"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "W"] <- "White"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "W                "] <- "White"
LouisvilleShootings$officer_race[LouisvilleShootings$officer_race == "A"] <- "Asian"
LouisvilleShootings$officer_sex[LouisvilleShootings$officer_sex == "M"] <- "Male"
LouisvilleShootings$officer_sex[LouisvilleShootings$officer_sex == "F"] <- "Female"

LouisvilleShootings$lethal[LouisvilleShootings$lethal == "Y"] <- TRUE
LouisvilleShootings$lethal[LouisvilleShootings$lethal == "N"] <- FALSE

#fixing capitalization
LouisvilleShootings$case_status <- tolower(LouisvilleShootings$case_status)
LouisvilleShootings$case_status <- str_to_title(LouisvilleShootings$case_status)

LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "SHOOTING INVESTIGATION - LMPD INVOLVED"] <- "Shooting investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "DEATH INVESTIGATION - LMPD INVOLVED"] <- "Death investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "SHOOTING INVESTIGATION- LMPD INVOLVED"] <- "Shooting investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "DEATH INVESTIGATION- LMPD INVOLVED"] <- "Death investigation LMPD involved"
LouisvilleShootings$investigation_type[LouisvilleShootings$investigation_type == "DEATH INVESTIGATION-LMPD INVOLVED"] <- "Death investigation LMPD involved"

#Making weapons less specific
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Shotgun"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Handgun"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Rifle"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "BB gun"] <- "Fake gun"

#Shootings to do
#figure out what to do about "continued"
#what does YS mean in lethal?
#clean years of service (get rid of years or make all have years, pluses? how to do this...)


#The rest of this is not complete, do not run!

#Clean citations
LouisvilleCitations <- tolower(LouisvilleCitations)
LouisvilleCitations <- str_to_title(LouisvilleCitations)





#Cleaning stops
#making a table with all relevant metadata for stops
LouisvilleStopsOriginal<-cbind.data.frame(stops$ID,stops$TYPE_OF_STOP,stops$CITATION_CONTROL_NUMBER,stops$ACTIVITY.RESULTS,stops$OFFICER_GENDER,stops$OFFICER_RACE,stops$OFFICER_AGE_RANGE,stops$ACTIVITY_DATE,stops$ACTIVITY_TIME,stops$ACTIVITY_LOCATION,stops$ACTIVITY_DIVISION,stops$ACTIVITY_BEAT,stops$DRIVER_GENDER,stops$DRIVER_RACE,stops$DRIVER_AGE_RANGE,stops$NUMBER.OF.PASSENGERS,stops$WAS_VEHCILE_SEARCHED,stops$REASON_FOR_SEARCH, stringsAsFactors=FALSE)
colnames(LouisvilleStops)<-(c("ID","type_of_stop","citation_control_number","activity.results","officer_gender","officer_race","officer_age_range","activity_date","activity_time","activity_location", "activity_division","activity_beat","driver_gender","driver_race","driver_age_range","number.of.passengers","was_vehcile_searched","reason_for_search"))

#make all null values = NA for stops
AllMetadata_stops_NA<-AllMetadata_stops
AllMetadata_stops_NA[AllMetadata_stops=="0"]<-NA
AllMetadata_stops_NA[AllMetadata_stops=="-"]<-NA

#Change race to be consisted with citations
AllMetadata_stops_FixRace<-AllMetadata_stops_NA
AllMetadata_stops_FixRace[AllMetadata_stops=="INDIAN/INDIA/BURMESE"]<-"INDIAN"
AllMetadata_stops_FixRace[AllMetadata_stops=="ASIAN/PACIFIC ISLANDER"]<-"ASIAN"

#write the file to a new location in clean data folder!
write.csv(AllMetadata_stops,"/Users/aryaajwani/Desktop/Research/policing/clean data/Louisville/stops_Louisville_clean",row.names = FALSE)
