#National EDA
library(dplyr)
library(stringr)
library(tidyverse)
install.packages("here")
library(here)
install.packages("stringi")
library("stringi")


#load in dataset
National<-read.csv(file = here('clean data/National/national_all.csv'), stringsAsFactors = TRUE)
View(National)

summary(National)
levels(National)


#adding officer group size and percent white columns

#cleaning columns such that there is no leading space and first letter is capitalized, so the code will work
National$officer_race <- trimws(National$officer_race)
National$officer_race <- stri_trans_totitle(National$officer_race)

National$officer_group_size <- str_count(National$officer_name, ',')    # counting commas
National$officer_group_size <- National$officer_group_size + 1    #adding one

National$white_officers <- str_count(National$officer_race, 'White')

National$percent_white <- National$white_officers / National$officer_group_size
National$percent_white <- National$percent_white * 100
#It is awful how many NAs there are, but at least now we can use what data we do have.
#Fixing specific issues
National$percent_white <- as.character(National$percent_white)
National$percent_white[National$percent_white== 500] <- 100
National$percent_white[National$percent_white== 200] <- 100
National$percent_white[National$percent_white== 150] <- 100



#numerical EDA - numerical colums: subject age, past shooting, percent white

#subject_age
National$subject_age <- as.numeric(National$subject_age)
var(National$subject_age, na.rm=TRUE)
sd(National$subject_age, na.rm=TRUE)
#histogram
hist(National$subject_age, breaks = 25)
#boxplot
boxplot(National$subject_age)
#Normality
ggplot(National, aes(sample=subject_age)) + 
  geom_qq () +
  geom_qq_line()


#Officer past shooting 
National$officer_past_shooting[National$officer_past_shooting== "No"] <- NA
National$officer_past_shooting[National$officer_past_shooting== "None"] <- NA
#National$officer_past_shooting[is.na(National$officer_past_shooting)] <- 0

National$officer_past_shooting <- as.numeric(National$officer_past_shooting)
var(National$officer_past_shooting, na.rm=TRUE)
sd(National$officer_past_shooting, na.rm=TRUE)
#histogram
hist(National$officer_past_shooting, breaks = 25)
#boxplot
boxplot(National$officer_past_shooting)
#Normality
ggplot(National, aes(sample=officer_past_shooting)) + 
  geom_qq () +
  geom_qq_line()


#percent white (this data is so terrible idk if we can count anything this tells us) 
National$percent_white <- as.numeric(National$percent_white)
var(National$percent_white, na.rm=TRUE)
sd(National$percent_white, na.rm=TRUE)
#histogram
hist(National$percent_white, breaks = 25)
#boxplot
boxplot(National$percent_white)
#Normality
ggplot(National, aes(sample=percent_white)) + 
  geom_qq () +
  geom_qq_line()



#graphs
ggplot(National, aes(subject_race))+
  geom_bar()


#Subject race by % of officer group that is white, raw numbers and percentages. Keeping in mind i wasnt able to extrapolate data for this very easily, so the sample size is really small. 
National$percent_white <- as.character(National$percent_white)  #making column characters because wack things happen when it is numeric

ggplot(data=subset(National, !is.na(percent_white)), 
       aes(x = percent_white,
           fill = subject_race))+
  geom_bar(position = "dodge")

ggplot(data=subset(National, !is.na(percent_white)), 
       aes(x = percent_white,
           fill = subject_race))+
  geom_bar(position = "fill")

#to show how bs this data is and why we can't really have any take aways
ggplot(National, 
       aes(x = percent_white,
           fill = subject_race))+
  geom_bar(position = "dodge")

#Subject race by officer group size, raw numbers and percentages
National$officer_group_size <- as.character(National$officer_group_size)

ggplot(data=subset(National, !is.na(officer_group_size)), 
       aes(x = officer_group_size,
           fill = subject_race))+
  geom_bar(position = "dodge")

ggplot(data=subset(National, !is.na(officer_group_size)), 
       aes(x = officer_group_size,
           fill = subject_race))+
  geom_bar(position = "fill")

# symptoms of mental illness by a few different things. Have to clean it better first
National$symptoms_of_mental_illness[National$symptoms_of_mental_illness== "Unknown"] <- NA
National$symptoms_of_mental_illness[National$symptoms_of_mental_illness== "unknown"] <- NA
National$symptoms_of_mental_illness[National$symptoms_of_mental_illness== "Unkown"] <- NA
National$symptoms_of_mental_illness[National$symptoms_of_mental_illness== "Unknown "] <- NA
National$symptoms_of_mental_illness[National$symptoms_of_mental_illness== "Drug or Alcohol Use"] <- "Drug or alcohol use"


ggplot(data=subset(National, !is.na(symptoms_of_mental_illness)), 
       aes(x = subject_race,
           fill = symptoms_of_mental_illness))+
  geom_bar(position = "dodge")

ggplot(data=subset(National, !is.na(symptoms_of_mental_illness)), 
       aes(x = subject_race,
           fill = symptoms_of_mental_illness))+
  geom_bar(position = "fill")
#Why do Black people have to lowest rates of officer reported symptoms of mental illness?

ggplot(data=subset(National, !is.na(symptoms_of_mental_illness)), 
       aes(x = officer_group_size,
           fill = symptoms_of_mental_illness))+
  geom_bar(position = "fill")

ggplot(data=subset(National, !is.na(symptoms_of_mental_illness)), 
       aes(x = percent_white,
           fill = symptoms_of_mental_illness))+
  geom_bar(position = "fill")



#Armed status
National$armed_status[National$armed_status== "Allegedly armed"] <- "Allegedly Armed"
National$armed_status[National$armed_status== "Unclear "] <- "Unclear"

ggplot(data=subset(National, !is.na(armed_status)), 
       aes(x = subject_race,
           fill = armed_status))+
  geom_bar(position = "fill")

ggplot(data=subset(National, !is.na(armed_status)), 
       aes(x = officer_group_size,
           fill = armed_status))+
  geom_bar(position = "fill")

ggplot(data=subset(National, !is.na(percent_white)), 
       aes(x = percent_white,
           fill = armed_status))+
  geom_bar(position = "dodge")




#threat level
ggplot(data=subset(National, !is.na(threat_level)), 
       aes(x = subject_race,
           fill = threat_level))+
  geom_bar(position = "fill")

ggplot(data=subset(National, !is.na(officer_group_size)), # This is one of the most clear analysis that we have seen.
       aes(x = officer_group_size,
           fill = threat_level))+
  geom_bar(position = "fill")




#fleeing
ggplot(data=subset(National, !is.na(fleeing)), 
       aes(x = subject_race,
           fill = fleeing))+
  geom_bar(position = "fill")

ggplot(data=subset(National, !is.na(officer_group_size)), 
       aes(x = officer_group_size,
           fill = fleeing))+
  geom_bar(position = "fill")



#Weapon
#using bins because there are otherwise way too many categories
National<-read.csv(file = here('clean data/National/national_all.csv'), stringsAsFactors = FALSE)

National$weapon_bin <- National$weapon

National$weapon_bin[str_detect(National$weapon_bin, "Toy Weapon")] <- "no real weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Gun")] <- "Gun"
National$weapon_bin[str_detect(National$weapon_bin, "Knife")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Sword")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Machete")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Weapon")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Ax")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Hachet")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Vehicle")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "Taser")] <- "Non-gun weapon"
#Same but not capitalized
National$weapon_bin[str_detect(National$weapon_bin, "gun")] <- "Gun"
National$weapon_bin[str_detect(National$weapon_bin, "knife")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "sword")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "machete")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "weapon")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "ax")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "hachet")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "vehicle")] <- "Non-gun weapon"
National$weapon_bin[str_detect(National$weapon_bin, "taser")] <- "Non-gun weapon"
National$weapon_bin[National$weapon_bin == "undetermined"] <- NA
National$weapon_bin[National$weapon_bin != c("Gun", "Non-gun weapon") ] <- "no real weapon"

#adding officer group size and percent white columns again
National$officer_race <- trimws(National$officer_race)
National$officer_race <- stri_trans_totitle(National$officer_race)
National$officer_group_size <- str_count(National$officer_name, ',')    # counting commas
National$officer_group_size <- National$officer_group_size + 1    #adding one
National$white_officers <- str_count(National$officer_race, 'White')
National$percent_white <- National$white_officers / National$officer_group_size
National$percent_white <- National$percent_white * 100
#Fixing specific issues
National$percent_white <- as.character(National$percent_white)
National$percent_white[National$percent_white== 500] <- 100
National$percent_white[National$percent_white== 200] <- 100
National$percent_white[National$percent_white== 150] <- 100

#Now back to the weapon EDA!
ggplot(data=subset(National, !is.na(weapon_bin)), 
       aes(x = subject_race,
           fill = weapon_bin))+
  geom_bar(position = "fill")

ggplot(data=subset(National, !is.na(officer_group_size)), 
       aes(x = officer_group_size,
           fill = weapon_bin))+
  geom_bar(position = "fill")


