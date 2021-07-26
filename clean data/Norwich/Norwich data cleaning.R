#Norwich Cleaning script

#calling libraries 
library (dplyr)
library (tidyr)
library(stringr)
library(tidyverse)
library(here)
install.packages("data.table")
install.packages("Here")
library(data.table)


#I want to load in my datasets
#set working directory
setwd("~/Desktop/policing/dirty data/Louisville")

#I want to call in my datasets
NorwichUOF2017<-read.csv(file = here('dirty data/Norwich/NorwichUOF2017.csv'), stringsAsFactors = FALSE, header= TRUE)
NorwichUOF2018<-read.csv(file = here('dirty data/Norwich/NorwichUOF2018.csv'), stringsAsFactors = FALSE)
NorwichUOF2019<-read.csv(file = here('dirty data/Norwich/NorwichUOF2019.csv'), stringsAsFactors = FALSE)

#The first row was the supposed to be the column names. Here is me fixing that for all years
colnames(NorwichUOF2017) <- NorwichUOF2017[1,]
NorwichUOF2017 = NorwichUOF2017[-1,]
colnames(NorwichUOF2018) <- NorwichUOF2018[1,]
NorwichUOF2018 = NorwichUOF2018[-1,]
colnames(NorwichUOF2019) <- NorwichUOF2019[1,]
NorwichUOF2019 = NorwichUOF2019[-1,]

#I'm going to merge the datasets together so I can do all of the cleaning once, instead of repeating it.
#First, i'm going to add a year column to each dataset so I will know which one is which
NorwichUOF2017$Year <- 2017
NorwichUOF2018$Year <- 2018
NorwichUOF2019$Year <- 2019

#Merging!
NorwichUOF <- bind_rows(NorwichUOF2017, NorwichUOF2018, NorwichUOF2019)

#There are a LOT of null rows (the dataset just had a bunch of rows hanging off the end, so i'm going to get ride of them.)
NorwichUOF <- NorwichUOF[!(NorwichUOF$`Call Type` == ""), ]

#removing the first column which is useless
NorwichUOF[ ,"Event #"] <- list(NULL)

#Fixing the column names so they are not multiple words
colnames(NorwichUOF) <- c("call_type", "time", "subject_weapon", "subject_age", "subject_gender", "subject_race", "subject_ethnicity", "alcohol_drugs", "PD_force_type", "number_of_officers", "arrest_or_protective_custody", "subject_injured", "officer_injured", "year")

#Turning F and M into Female and Male
NorwichUOF$subject_gender[NorwichUOF$subject_gender== "F"] <- "Female"
NorwichUOF$subject_gender[NorwichUOF$subject_gender== "M"] <- "Male"

#Turning None into NA
NorwichUOF[NorwichUOF== "None"] <- NA

#Fix one cell to match the pattern
NorwichUOF$subject_injured[NorwichUOF$subject_injured == "Complained of injured wrist"] <- "Yes-Complained of injured wrist"

#separate subject_injured into subject_injured and subject_injury_description.
#This requires seperating it by comma and dash, then merging the 2 resulting columns
NorwichUOF <-separate(NorwichUOF, subject_injured, c('subject_injured', 'injury_description2'), sep="-", extra = "merge")
NorwichUOF <-separate(NorwichUOF, subject_injured, c('subject_injured', 'injury_description1'), sep=", ", extra = "merge")
NorwichUOF$injury_description2[is.na(NorwichUOF$injury_description2)] <- ""
NorwichUOF$injury_description1[is.na(NorwichUOF$injury_description1)] <- ""
NorwichUOF$subject_injury_description <- paste(NorwichUOF$injury_description1, NorwichUOF$injury_description2)

# then remove unneccesary columns and fix NAs
NorwichUOF[ ,c("injury_description1", "injury_description2")] <- list(NULL)
NorwichUOF$subject_injury_description[NorwichUOF$subject_injury_description == " "] <- NA

#then clean subject_injury_description such that there is no leading space and first letter is capitalized
NorwichUOF$subject_injury_description <- trimws(NorwichUOF$subject_injury_description)
NorwichUOF$subject_injury_description <- tolower(NorwichUOF$subject_injury_description)
substr(NorwichUOF$subject_injury_description, 1, 1) <- toupper(substr(NorwichUOF$subject_injury_description, 1, 1))

#Repeat with officer_injured
NorwichUOF <-separate(NorwichUOF, officer_injured, c('officer_injured', 'injury_description2'), sep="-", extra = "merge")
NorwichUOF <-separate(NorwichUOF, officer_injured, c('officer_injured', 'injury_description1'), sep=", ", extra = "merge")
NorwichUOF$injury_description2[is.na(NorwichUOF$injury_description2)] <- ""
NorwichUOF$injury_description1[is.na(NorwichUOF$injury_description1)] <- ""
NorwichUOF$officer_injury_description <- paste(NorwichUOF$injury_description1, NorwichUOF$injury_description2)
NorwichUOF[ ,c("injury_description1", "injury_description2")] <- list(NULL)
NorwichUOF$officer_injury_description[NorwichUOF$officer_injury_description == " "] <- NA
NorwichUOF$officer_injury_description <- trimws(NorwichUOF$officer_injury_description)
NorwichUOF$officer_injury_description <- tolower(NorwichUOF$officer_injury_description)
substr(NorwichUOF$officer_injury_description, 1, 1) <- toupper(substr(NorwichUOF$officer_injury_description, 1, 1))


#exporting to clean data folder 
write.csv(NorwichUOF,here("clean data","Norwich","norwich_UOF.csv"),row.names = FALSE)