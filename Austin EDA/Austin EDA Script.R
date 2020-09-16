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
YrsOfServicewoNA = as.data.frame(YrsOfServicewoNA)
sd(YrsOfServicewoNA)


##graphs
hist(UOF_EDA$officer.Yrs.of.service)
hist(Shootings_EDA$Officer.Age)

#outliers

boxplot(Shootings_EDA$Officer.Age)
boxplot(UOF_EDA$officer.Yrs.of.service)


#normality

ggplot(Shootings_EDA, aes(sample = Officer.Age)) +
  geom_qq() +
  geom_qq_line()

ggplot(UOF_EDA, aes(sample = officer.Yrs.of.service)) +
  geom_qq() +
  geom_qq_line()


