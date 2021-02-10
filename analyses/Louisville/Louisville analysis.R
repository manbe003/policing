#Louisville analysis
#ideas
install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 

LouisvilleShootings<-read.csv(file = here('clean data/Louisville/LouisvilleShootings.csv'), stringsAsFactors = FALSE)
View(DallasIncidents)

DallasIncidents<-read.csv(file = here('dirty data/Dallas/Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.zip'), stringsAsFactors = FALSE)

sum(is.na(DallasShootings$officer_badge_number))

#the function
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

LouisvilleShootings <- OfficerGroupSize(LouisvilleShootings, "PIU_number", LouisvilleShootings$PIU_number)

#questions
#rare cases of multiple victims per one officer. Should i switch PUI_number with subject_name? what if the same person gets two different citations then? And there are a good deal of NAs in that column


#Ok. That worked. Now lets try to make a function for the % white/black column . This one will be more difficult. 

# Does not work, but close? -- LouisvilleShootings$white_officers <- ave(LouisvilleShootings$officer_race == "White", LouisvilleShootings$PIU_number)

#This works, but there is probably a better way. Still it took me hours and I am quite proud of it.

OfficerRaceGroup <- LouisvilleShootings %>%  
  group_by(PIU_number, officer_race) %>% 
  summarise(Freq = n()) 

OfficerRaceGroup<-OfficerRaceGroup[sample_n(OfficerRaceGroup$officer_race=="White")]

OfficerRaceGroup<-OfficerRaceGroup[!(OfficerRaceGroup$officer_race=="Black"),]
OfficerRaceGroup<-OfficerRaceGroup[!(OfficerRaceGroup$officer_race=="Asian"),]
OfficerRaceGroup[,"officer_race"] <- list(NULL)
colnames(OfficerRaceGroup) <- c("PIU_number", "white_officers")
LouisvilleShootings <- merge(OfficerRaceGroup, LouisvilleShootings, by = "PIU_number", all.y = TRUE)

LouisvilleShootings$white_officers[is.na(LouisvilleShootings$white_officers)] <- 0
LouisvilleShootings$percent_white <- LouisvilleShootings$white_officers / LouisvilleShootings$officer_group_size
LouisvilleShootings$percent_white <- LouisvilleShootings$percent_white * 100

LouisvilleShootings$percent_white <- as.factor(LouisvilleShootings$percent_white)

LouisvilleShootingsUnique<-subset(LouisvilleShootings, !duplicated(PIU_number))

ggplot(LouisvilleShootingsUnique, aes(percent_white))+
  geom_bar()

#This works. Now lets make it into a fuction! The function does not work yet, there are problems we=ith the colum names having "" or not i think. also $ dont work. help?
PercentWhiteCol <- function(dataset, numbercol, racecol){
  OfficerRaceGroup <- dataset %>%  
    group_by(numbercol, racecol) %>% 
    summarise(Freq = n()) 
  
  OfficerRaceGroup<-OfficerRaceGroup[!(OfficerRaceGroup$racecol=="Black"),]
  OfficerRaceGroup<-OfficerRaceGroup[!(OfficerRaceGroup$racecol=="Asian"),]
  OfficerRaceGroup[,racecol] <- list(NULL)
  colnames(OfficerRaceGroup) <- c(numbercol, "white_officers")
  dataset <- merge(OfficerRaceGroup, dataset, by = numbercol, all.y = TRUE)
  
  dataset$white_officers[is.na(dataset$white_officers)] <- 0
  dataset$percent_white <- dataset$white_officers / dataset$officer_group_size
  dataset$percent_white <- dataset$percent_white * 100
  return(dataset)
}

LouisvilleShootings <- PercentWhiteCol(LouisvilleShootings, "PIU_number", "officer_race")


#this is all just brainstorming and varius ideas. Ignore below. 
for i in 1:length(DS)
if [id == i]
LouisvilleShootings$white_officers <- sum(LouisvilleShootings$officer_race == White)


ds$n <- sum(incident_number==x)
X== incident number for every row.
For i in 1:length(unique(ID)
unique (id), length (table)
Group IDS, and count them
Then match by ID

library(dplyr)

x = unique(dat$PIU_number)
y = list()
                  
for (i in 1:length(x)){
y[i] = sum(dat$PIU_number==x[i])  
}


