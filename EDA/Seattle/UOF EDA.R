#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

#I want to call in my datasets (citations).
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = TRUE)

#summary data
summary(UOF)

#categorical data analysis
levels(UOF$Subject_Gender)
levels(UOF$Subject_Race)
levels(UOF$Incident_Type)

ggplot(UOF, aes(Subject_Gender))+
  geom_bar()

ggplot(UOF, aes(Subject_Race))+
  geom_bar()

ggplot(UOF, aes(Incident_Type))+
  geom_bar()

ggplot(UOF,
       aes(x = Subject_Race,
           fill = Incident_Type))+
  geom_bar(position = "dodge")
