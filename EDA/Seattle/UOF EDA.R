#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = TRUE)

#summary data
summary(UOF)

#categorical data analysis
levels(UOF$Subject_Race)
levels(UOF$Subject_Gender)
levels(UOF$Incident_Type)
levels(UOF$Precinct)

ggplot(UOF, aes(Subject_Race))+
  geom_bar()

ggplot(UOF, aes(Subject_Gender))+
  geom_bar()

ggplot(UOF, aes(Incident_Type))+
  geom_bar()

ggplot(UOF, aes(Precinct))+
  geom_bar()

