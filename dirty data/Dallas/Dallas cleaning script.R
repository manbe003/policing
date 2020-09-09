# Dallas data cleaning script
library(dplyr)
library(tidyr)
library(stringr)
setwd("~/Desktop/policing/dirty data/Dallas")
DallasShootings <- read.csv("DallasPoliceShootings.csv", stringsAsFactors = FALSE)
View(DallasShootings)

# I want to rename the columns
colnames(DallasShootings) <- c("case", "date", "location", "result_of_shot_in_subject", "subject_weapon", "subject_info", "officer_info", "grand_jury_disposition", "attorney_general_forms", "summary_url", "geolocation")

# individually changing the messed up values, double names, and unknowns. I dont know if there is a better way to do this
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
DallasShootings[84, 7] = "Carballo, Rolando Jr. L/M; Alexander, Alphonse B/M"

#I need to seperate subject_info 
DallasShootings <- data.frame(DallasShootings,do.call(rbind,str_split(DallasShootings$subject_info,",")))
colnames(DallasShootings)[which(names(DallasShootings) == "X1")] <- "subject_last_name"
colnames(DallasShootings)[which(names(DallasShootings) == "X2")] <- "first_name_race_gender"
DallasShootings <- data.frame(DallasShootings,do.call(rbind,str_split(DallasShootings$first_name_race_gender," ")))
colnames(DallasShootings)[which(names(DallasShootings) == "X2")] <- "subject_first_name"
colnames(DallasShootings)[which(names(DallasShootings) == "X3.1")] <- "race_gender"
DallasShootings <- data.frame(DallasShootings,do.call(rbind,str_split(DallasShootings$race_gender,"/")))
colnames(DallasShootings)[which(names(DallasShootings) == "X1.1")] <- "subject_race"
colnames(DallasShootings)[which(names(DallasShootings) == "X2")] <- "subject_gender"
DallasShootings$subject_name <- paste(DallasShootings$subject_first_name, DallasShootings$subject_last_name)
DallasShootings[ ,c(8,9,13,14,15,16,18,19,20,12,17)] <- list(NULL)



DallasShootings <- data.frame(DallasShootings,do.call(rbind,str_split(DallasShootings$officer_info,",")))
colnames(DallasShootings)[which(names(DallasShootings) == "X1")] <- "officer_last_name"
colnames(DallasShootings)[which(names(DallasShootings) == "X2")] <- "first_name_race_gender"
DallasShootings <- data.frame(DallasShootings,do.call(rbind,str_split(DallasShootings$first_name_race_gender," ")))
colnames(DallasShootings)[which(names(DallasShootings) == "X2")] <- "officer_first_name"
colnames(DallasShootings)[which(names(DallasShootings) == "X3.1")] <- "race_gender"
DallasShootings <- data.frame(DallasShootings,do.call(rbind,str_split(DallasShootings$race_gender,"/")))
colnames(DallasShootings)[which(names(DallasShootings) == "X1.1")] <- "officer_race"
colnames(DallasShootings)[which(names(DallasShootings) == "X2")] <- "officer_gender"
DallasShootings$officer_name <- paste(DallasShootings$officer_first_name, DallasShootings$officer_last_name)
DallasShootings[ ,c(13:37)] <- list(NULL)




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


