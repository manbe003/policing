#National dataset cleaning
library (dplyr)
library (tidyr)
library(stringr)
library(tidyverse)
library(here)
install.packages("data.table")
install.packages("Here")
library(data.table)


National<-read.csv(file = here('dirty data/National/MPVDatasetDownload.csv'), stringsAsFactors = FALSE, header= TRUE)

View(National)

#getting rid of extra rows at the end
National[ ,c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9", "X.10", "X.11", "X.12", "X.13")] <- list(NULL)

#renaming columns
colnames(National) <- c("subject_name", "subject_age", "subject_gender", "subject_race", "subject_image_url", "date", "address", "city", "state", "zipcode", "county", "agency", "ORI_agency", "cause_of_death", "description", "official_disposition", "criminal_charges", "link", "symptoms_of_mental_illness", "armed_status", "weapon", "threat_level", "fleeing", "body_camera", "WaPo_ID", "off_duty", "geography", "MPV_ID", "fatal_encounters_ID", "encounter_type", "reason_for_encounter", "officer_name", "officer_race", "officer_past_shooting", "call_for_service")


#getting rid of more unneeded columns mainly councerning various IDs
National[ ,c("subject_image_url", "ORI_agency", "link", "WaPo_ID", "MPV_ID", "fatal_encounters_ID", "geography" )] <- list(NULL)

#turning blanks into NAs
National[National== ""] <- NA

#turing unknowns into NAs
National$subject_name[National$subject_name== "Unknown"] <- NA
National$subject_age[National$subject_age== "Unknown"] <- NA
National$subject_race[National$subject_race== "Unknown race"] <- NA
National$subject_gender[National$subject_gender== "Unknown"] <- NA
National$call_for_service[National$call_for_service== "Unavailable"] <- NA

#Making the values capitalized correctly
substr(National$weapon, 1, 1) <- toupper(substr(National$weapon, 1, 1))
substr(National$body_camera, 1, 1) <- toupper(substr(National$body_camera, 1, 1))
substr(National$threat_level, 1, 1) <- toupper(substr(National$threat_level, 1, 1))
substr(National$fleeing, 1, 1) <- toupper(substr(National$fleeing, 1, 1))
substr(National$reason_for_encounter, 1, 1) <- toupper(substr(National$reason_for_encounter, 1, 1))

#Turning National into NationalAll since i will now be only working with a subset of it
NationalAll <- National

#Making a subset of the table that only includes rows with officer name
NationalOfficerName <- 
  National %>%
   filter(!is.na(National$officer_name))

#Making a officer group size column based on the number of commas + 1  in officer name 
NationalOfficerName$officer_group_size <- str_count(NationalOfficerName$officer_name, ',')    # counting commas
NationalOfficerName$officer_group_size <- NationalOfficerName$officer_group_size + 1    #adding one

#exporting datasets to clean folder 
write.csv(NationalOfficerName,here("clean data","National","national_officer_name.csv"),row.names = FALSE)
write.csv(NationalAll,here("clean data","National","national_all.csv"),row.names = FALSE)


