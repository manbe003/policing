#Austin EDA

setwd("C:/Users/katie/Desktop/policing/clean data/Austin")

#load library
library(tidyverse)


#Descriptive data
UOF_EDA<-read.csv(file='UseOfForce_Austin.csv', stringsAsFactors = FALSE)
Shootings_EDA<-read.csv(file='Shootings_Austin.csv', stringsAsFactors = FALSE)
Citations_EDA<-read.csv(file='Citations_Austin.csv', stringsAsFactors = FALSE)

summary(UOF_EDA)
summary(Shootings_EDA)
summary(Citations_EDA)


#standard deviations
sd(Shootings_EDA$Officer.Age)

#fixing NA values in UOF officer yrs of service
UOF_EDA$officer.Yrs.of.serviceNA = as.numeric(UOF_EDA$officer.Yrs.of.service)
YrsOfServicewoNA = na.exclude(UOF_EDA$officer.Yrs.of.serviceNA)
sd(YrsOfServicewoNA)

#variation
var(Shootings_EDA$Officer.Age)
var(YrsOfServicewoNA)

##graphs
hist(as.numeric(UOF_EDA$officer.Yrs.of.service), breaks = 50)
hist(as.numeric(Shootings_EDA$Officer.Age), breaks = 50)

#outliers

boxplot(UOF_EDA$officer.Yrs.of.service)
boxplot(Shootings_EDA$Officer.Age)


#normality

ggplot(Shootings_EDA, aes(sample = Officer.Age)) +
  geom_qq() +
  geom_qq_line()

ggplot(UOF_EDA, aes(sample = officer.Yrs.of.service)) +
  geom_qq() +
  geom_qq_line()


##categorical data

#loading files
CatUOF_EDA<-read.csv(file='UseOfForce_Austin.csv', stringsAsFactors = TRUE)
CatShootings_EDA<-read.csv(file='Shootings_Austin.csv', stringsAsFactors = TRUE)
CatCitations_EDA<-read.csv(file='Citations_Austin.csv', stringsAsFactors = TRUE)

summary(CatUOF_EDA)
summary(CatCitations_EDA)
summary(CatShootings_EDA)


#levels for UOF
levels(CatUOF_EDA$subject.race)
levels(CatUOF_EDA$subject.sex) 
levels(CatUOF_EDA$Subject.Ethnicity)
levels(CatUOF_EDA$Area.Command)       

#graphs for UOF

ggplot(CatUOF_EDA, aes(subject.race)) +
  geom_bar()

ggplot(CatUOF_EDA, aes(subject.sex)) +
  geom_bar()

ggplot(CatUOF_EDA, aes(Subject.Ethnicity)) +
  geom_bar()

ggplot(CatUOF_EDA, aes(Area.Command)) +
  geom_bar()
       
       
       
       
       
       