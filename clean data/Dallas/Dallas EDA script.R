#Dallas EDA script
install.packages("tidyverse")
library(tidyverse)
setwd("~/Desktop/policing/clean data/Dallas")
Dallas.Shootings <- read.csv("Dallas_shootings.csv", stringsAsFactors = TRUE)
Dallas.R2R <- read.csv("Dallas_R2R.csv", stringsAsFactors = TRUE)



summary(Dallas.R2R)
summary(Dallas.Shootings)

#graphs (all for shootings). I have no relevant numeric data, so can skip normality test, histogram, box plots and stuff for now.

#bar graphs of officer race for shootings
ggplot(Dallas.Shootings, aes(officer_race)) +
  geom_bar()
#bar graphs of subject race for shootings
ggplot(Dallas.Shootings, aes(subject_race)) +
  geom_bar()


#subject weapon by race
ggplot(Dallas.Shootings, 
       aes(x = subject_race, 
           fill = subject_weapon)) + 
  geom_bar(position = "dodge")


#stacked graph of subject weapon by race, using percentages
ggplot(Dallas.Shootings, 
       aes(x = subject_race, 
           fill = subject_weapon)) + 
  geom_bar(position = "fill")

#same variables but using raw numbers instead of percentages.
ggplot(Dallas.Shootings, 
       aes(x = subject_race, 
           fill = subject_weapon)) + 
  geom_bar(position = "stack")

#the percentages of subjects of each race shot at by officers of each race
ggplot(Dallas.Shootings, 
       aes(x = subject_race, 
           fill = officer_race)) + 
  geom_bar(position = "fill")

#subjects weapon by officer race, percentages.
ggplot(Dallas.Shootings, 
       aes(x = officer_race, 
           fill = subject_weapon)) + 
  geom_bar(position = "fill")