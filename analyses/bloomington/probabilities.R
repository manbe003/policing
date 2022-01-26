#loading libraries
library(here)
library(epitools)
library(tidyverse)
library(dplyr)
library(tidyr)

#loading in datasets
UOF<- read.csv(file=here('clean data/Bloomington/Bloomington_UOF.csv'), stringsAsFactors = FALSE)

#making date and time one combined column to later match rows based on the same date and time
UOF2<- UOF


#collapsing the force level row so it puts each force level with the same incident number in the same row seperated by commas
UOF3 <- UOF2 %>%
  dplyr::group_by(Event) %>%
  summarise(Force.Level = paste(Force.Level, collapse=",  "))


#collapsing the Officer ID row so it puts each officer ID with the same incident DateTime in the same row seperated by commas
UOF4 <- UOF2 %>%
  dplyr::group_by(Event) %>%
  summarise(Suspect.Race = paste(Suspect.Race, collapse=",  "))


#combing them so all the needed data is in one dataframe
UOF3$SuspectRace <- paste(UOF4$Suspect.Race)  

#Binning Force Type (1- no weapon, 2- non lethal weapon, 3- lethal force)
UOF_All_FixLevels <- UOF3
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`Force.Level` = case_when(
    str_detect(`Force.Level`, "3") ~ "3",
    str_detect(`Force.Level`, "2") ~ "2",
    str_detect(`Force.Level`, "1") ~ "1",
    TRUE ~ `Force.Level`
  ))

UOF_All_FixLevels$Force.Level=gsub("*.NA.*", NA, UOF_All_FixLevels$Force.Level)

#binning number of officers into 1,2, or 3+
OfficerBinning <- function (dataframe, dataframecol, separator){
  dataframe['Number of Officers'] <- NA
  dataframe['Binning Number of Officers'] <- NA
  dataframe$`Number of Officers` <- str_count(dataframecol, coll(separator))+1
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="1"]<- "1"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="2"]<- "2"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers` > "2"]<- "3+"
  return(dataframe)
  
}

UOF_All_FixLevels<- OfficerBinning(UOF_All_FixLevels,UOF_All_FixLevels$SuspectRace, ",  ")


probability<-function(df, colnum){
  x<-cbind(table(df[,colnum]))
  

  
  print("odds of escalating past force of 1")
  print(sum(x[2,1])/sum(x[1:2,1]))
}

probability(UOF_All_FixLevels, 2)

UOF_FixLevels<-split(UOF_All_FixLevels, f=(UOF_All_FixLevels$`Binning Number of Officers`))
probability(UOF_FixLevels$`1`,2)
probability(UOF_FixLevels$`2`,2)
probability(UOF_FixLevels$`3`,2)


#now looking to do a chi-square test to assess difference
#first make a table of number of officers by force type
dt <- table(UOF_All_FixLevels$`Binning Number of Officers`,UOF_All_FixLevels$Force.Level)

dt

#Graph
balloonplot(t(dt), main ="Force.Level", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dt, shade = TRUE, las=2,main = "Force.Level")

#not enough power to do a chi square test
