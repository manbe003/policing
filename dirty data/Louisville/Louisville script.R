#first I want to call libraries 
library (dplyr)
library (tidyr)
library(stringr)

#set working directory
setwd("~/Desktop/policing/dirty data/Louisville")

#I want to call in my datasets (citations, stops, shootings)

LouisvilleStops<-read.csv(file = file.path('dirty data/Louisville/Louisville stops.csv'), stringsAsFactors = FALSE)

LouisvilleShootings<-read.csv(file = file.path('dirty data/Louisville/all_louisville_OIS.csv'), stringsAsFactors = FALSE)

#LouisvilleCitations <- read.csv(unz("UniformCitationData .csv.zip", "UniformCitationData .csv"))
#LouisvilleCitations[ ,c("AGENCY_DESC", "ID", "CITATION_YEAR", "ASCF_CODE", "STATUTE", "UCR_CODE", "PERSONS_HOME_CITY", "PERSONS_HOME_STATE", "PERSONS_HOME_ZIP")] <- list(NULL)
#write.csv(LouisvilleCitations,"~/Desktop/policing/dirty data/Louisville/Louisville_citations.csv",row.names = FALSE)

LouisvilleCitations <- read.csv(file = file.path('dirty data/Louisville/Louisville_citations.csv'), stringsAsFactors = FALSE)

#Clean Shootings

#delete subject ethnicity, as it is all none, N, or U
LouisvilleShootings[,c("subject_ethnicity", "officer_ethnicity")] <- list(NULL)
LouisvilleShootings<-LouisvilleShootings[1:100,]

#LouisvilleShootings[LouisvilleShootings == "U"] <- NA
#LouisvilleShootings[LouisvilleShootings == "UNKNOWN"] <- NA

#Replace B with Black, W with White, Unknown with NA,blank values with NA, ect
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

LouisvilleShootings$lethal[LouisvilleShootings$lethal == "Y"] <- TRUE
LouisvilleShootings$lethal[LouisvilleShootings$lethal == "N"] <- FALSE

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

#Making weapons less specific
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Shotgun"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Handgun"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "Rifle"] <- "Firearm"
LouisvilleShootings$subject_weapon[LouisvilleShootings$subject_weapon == "BB gun"] <- "Fake gun"

#Making a function that fills the blank values with the one above (thanks, Stack Overflow!)
fillTheBlanks <- function(x, missing=""){
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
}
#Using this function on all of the necessary columns. This still needs a bit of work.
LouisvilleShootings$month <- fillTheBlanks(LouisvilleShootings$month)
LouisvilleShootings$PIU_number <- fillTheBlanks(LouisvilleShootings$PIU_number)
LouisvilleShootings$incident_number <- fillTheBlanks(LouisvilleShootings$incident_number)
LouisvilleShootings$day <- fillTheBlanks(LouisvilleShootings$day)
LouisvilleShootings$time <- fillTheBlanks(LouisvilleShootings$time)
LouisvilleShootings$division <- fillTheBlanks(LouisvilleShootings$division)
LouisvilleShootings$investigation_type <- fillTheBlanks(LouisvilleShootings$investigation_type)
LouisvilleShootings$case_status <- fillTheBlanks(LouisvilleShootings$case_status)
LouisvilleShootings$subject_name <- fillTheBlanks(LouisvilleShootings$subject_name)
LouisvilleShootings$subject_age <- fillTheBlanks(LouisvilleShootings$subject_age)
LouisvilleShootings$subject_weapon <- fillTheBlanks(LouisvilleShootings$subject_weapon)
LouisvilleShootings$officer_age <- fillTheBlanks(LouisvilleShootings$officer_age)
LouisvilleShootings$officer_name <- fillTheBlanks(LouisvilleShootings$officer_name)
LouisvilleShootings$years_of_service <- fillTheBlanks(LouisvilleShootings$years_of_service)
LouisvilleShootings$address <- fillTheBlanks(LouisvilleShootings$address)
LouisvilleShootings$lethal <- fillTheBlanks(LouisvilleShootings$lethal)
LouisvilleShootings$lethal <- fillTheBlanks(LouisvilleShootings$lethal, missing = "Continued")





#Questions
#figure out what to do about 20+ and the one 2/3 colum in years of service
#I think YS means self inflicted death, what should I do?

#analysis ideas - are white officers more likely to shoot/hurt BIPOC if they are in all white groups?



View(LouisvilleCitations)
#Clean citations
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

