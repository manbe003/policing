#loading libraries
library(here)
library(epitools)
library(tidyverse)


#loading datasets
UOF<-read.csv(file=here('clean data/Dallas/Dallas_R2R.csv'), stringsAsFactors = FALSE)


UOF_All_FixLevels <- UOF
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`force_type` = case_when(
    str_detect(`force_type`, "Deadly Force") ~ "3",
    str_detect(`force_type`, "40mm") ~ "1",
    str_detect(`force_type`, "Weapon display") ~ "1",
    str_detect(`force_type`, "Taser display") ~ "1",
    str_detect(`force_type`, "Baton") ~ "2",
    str_detect(`force_type`, "OC Spray") ~ "2",
    str_detect(`force_type`, "K-9") ~ "2",
    str_detect(`force_type`, "Pepperball") ~ "2",
    str_detect(`force_type`, "Taser") ~ "2",
    str_detect(`force_type`, "Weapon") ~ "2",
    str_detect(`force_type`, "Strike") ~ "1",
    str_detect(`force_type`, "Grabbed") ~ "1",
    str_detect(`force_type`, "Take Down") ~ "1",
    str_detect(`force_type`, "Held") ~ "1",
    str_detect(`force_type`, "Stacked Weight") ~ "1",
    str_detect(`force_type`, "Arm Bar") ~ "1",
    str_detect(`force_type`, "Hand Control") ~ "1",
    str_detect(`force_type`, "Verbal Command") ~ "1",
    str_detect(`force_type`, "Balance Displacement") ~ "1",
    str_detect(`force_type`, "Locks") ~ "1",
    str_detect(`force_type`, "Pressure Points") ~ "1",
    str_detect(`force_type`, "Bar Hammer") ~ "1",
    str_detect(`force_type`, "Legs") ~ "1",
    str_detect(`force_type`, "Restraint") ~ "1",
    str_detect(`force_type`, "Pursuit") ~ "1",
    str_detect(`force_type`, "Take down") ~ "1",
    str_detect(`force_type`, "Pushed") ~ "1",
    str_detect(`force_type`, "Handcuffing") ~ "1",
    str_detect(`force_type`, "Arms") ~ "1",
    str_detect(`force_type`, "Tripped") ~ "1",
    TRUE ~ `force_type`
  ))

UOF_All_FixLevels[UOF_All_FixLevels=="Mach 1"]<-NA
UOF_All_FixLevels[UOF_All_FixLevels=="Mach 2"]<-NA
UOF_All_FixLevels[UOF_All_FixLevels=="Mach 3"]<-NA
UOF_All_FixLevels[UOF_All_FixLevels=="Other"]<-NA
UOF_All_FixLevels[UOF_All_FixLevels=="Other, Other"]<-NA



