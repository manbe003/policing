#Louisville EDA
library(dplyr)
library(stringr)
library(tidyverse)


#load in datasets
LouisvilleStops<-read.csv(file = here('clean data/Louisville/LouisvilleStops.csv'), stringsAsFactors = TRUE)
LouisvilleShootings<-read.csv(file = here('clean data/Louisville/LouisvilleShootings.csv'), stringsAsFactors = TRUE)
setwd("~/Desktop/policing/clean data/Louisville")
LouisvilleCitations <- read.csv(unz('LouisvilleCitations.csv.zip', 'LouisvilleCitations.csv'), stringsAsFactors = TRUE)

View(LouisvilleCitations)
summary(LouisvilleShootings)
levels(LouisvilleShootings)

summary(LouisvilleStops)
levels(LouisvilleStops)

summary(LouisvilleShootings)
levels(LouisvilleShootings)


#numerical EDA
#Louisville Shootings

#shootings numerical columns, subject_age, officer_age, years_of_service
var(LouisvilleShootings$subject_age, na.rm=TRUE)
sd(LouisvilleShootings$subject_age, na.rm=TRUE)
var(LouisvilleShootings$officer_age, na.rm=TRUE)
sd(LouisvilleShootings$officer_age, na.rm=TRUE)
var(LouisvilleShootings$years_of_service, na.rm=TRUE)
sd(LouisvilleShootings$years_of_service, na.rm=TRUE)

#subject age
#histogram
hist(LouisvilleShootings$subject_age, breaks = 25)
#boxplot
boxplot(LouisvilleShootings$subject_age)
#Normality
ggplot(LouisvilleShootings, aes(sample=subject_age)) + 
  geom_qq () +
  geom_qq_line()
  
#officer age
#histogram
hist(LouisvilleShootings$officer_age, breaks = 25)
#boxplot
boxplot(LouisvilleShootings$officer_age)
#Normality
ggplot(LouisvilleShootings, aes(sample=officer_age)) + 
  geom_qq () +
  geom_qq_line()

#years of service
#histogram
hist(LouisvilleShootings$years_of_service, breaks = 25)
#boxplot
boxplot(LouisvilleShootings$years_of_service)
#Normality
ggplot(LouisvilleShootings, aes(sample=years_of_service)) + 
  geom_qq () +
  geom_qq_line()


#Stops. Only relevant numeric column is number of passengers 
var(LouisvilleStops$number_of_passengers, na.rm=TRUE)
sd(LouisvilleStops$number_of_passengers, na.rm=TRUE)
#histogram
hist(LouisvilleStops$number_of_passengers, breaks = 25)
#boxplot
boxplot(LouisvilleStops$number_of_passengers)
#Normality
ggplot(LouisvilleStops$number_of_passengers, aes(sample=number_of_passengers)) + 
  geom_qq () +
  geom_qq_line()

#Citations. Only numeric column is subject age.
var(LouisvilleCitations$subject_age, na.rm=TRUE)
sd(LouisvilleCitations$subject_age, na.rm=TRUE)
#histogram
hist(LouisvilleCitations$subject_age, breaks = 25)
#boxplot
boxplot(LouisvilleCitations$subject_age)
#Normality
ggplot(LouisvilleCitations, aes(sample=subject_age)) + 
  geom_qq () +
  geom_qq_line()

#graphs

#Shootings


LouisvilleShootingsUnique<-

ggplot(LouisvilleShootings, aes(subject_race))+
  geom_bar()


ggplot(LouisvilleShootings,
       aes(x = subject_race,
           fill = officer_race))+
  geom_bar(position = "dodge")

#Stops

table(LouisvilleStops$officer_age_range)

ggplot(LouisvilleStops, aes(driver_race))+
  geom_bar()

ggplot(LouisvilleStops, aes(number_of_passengers))+
  geom_bar()

ggplot(LouisvilleStops, aes(officer_race))+
  geom_bar()

ggplot(LouisvilleStops, aes(type_of_stop))+
  geom_bar()


ggplot(LouisvilleStops,
       aes(x = driver_race,
           fill = driver_gender))+
  geom_bar(position = "dodge")


ggplot(LouisvilleStops,
       aes(x = driver_race,
           fill = officer_race))+
  geom_bar(position = "dodge")

ggplot(LouisvilleStops,
       aes(x = driver_gender,
           fill = officer_gender))+
  geom_bar(position = "dodge")


#if anything, take a look at this graph. it took me about 3 hours overall and it is beautiful but also deeply troubling.
ggplot(data=subset(LouisvilleStops, !is.na(reason_for_search)),
       aes(x = reason_for_search,
           fill = driver_race))+
  geom_bar(position = "fill") +
  coord_flip()

ggplot(data=subset(LouisvilleStops, !is.na(reason_for_search)),
       aes(x = reason_for_search,
           fill = driver_gender))+
  geom_bar(position = "fill") +
  coord_flip()

ggplot(data=subset(LouisvilleStops, !is.na(reason_for_search)),
       aes(x = driver_race,
           fill = type_of_stop))+
  geom_bar(position = "fill")


#Citations

ggplot(LouisvilleCitations, aes(subject_race))+
  geom_bar()

ggplot(LouisvilleCitations, aes(subject_gender))+
  geom_bar()

ggplot(LouisvilleCitations,
       aes(x = subject_age,
           fill = subject_race))+
  geom_bar(position = "dodge")

#Take a look at these two as well. They took forever. 
ggplot(data=subset(LouisvilleCitations, !(LouisvilleCitations$citation_category=="OTHER")),
       aes(x = citation_category,
           fill = subject_race))+
  geom_bar(position = "fill") +
  coord_flip()

ggplot(data=subset(LouisvilleCitations, !(LouisvilleCitations$citation_category=="OTHER")),
       aes(x = subject_race,
           fill = citation_category))+
  geom_bar(position = "fill")


#Boolean. It works (I think), but I don't know if it is right. 

LouisvilleDemographics<- data.frame("White"= .656, "Black" = .236, "Hispanic"= .056, "Asian"= .027, "Alaskan/American Native"= .002) 

#The function
BooleanLouisville <- function(demodata, racecol){
  
  listrace = unique(racecol)
  listrace<-listrace[!is.na(listrace)]
  listrace<-listrace[!(listrace =="Middle Eastern")]
  listrace<-listrace[!(listrace =="Indian")]
  
  race.boolean=matrix(ncol=length(listrace), nrow=length(racecol))
  
  for(i in 1:length(listrace)){
    race.boolean[,i] = racecol == listrace[i]
  }
  
  colnames(race.boolean) <- c(print(listrace))
  
  race.boolean <- as.data.frame(race.boolean)
  
  l=length(race.boolean[,1])
  
  for(i in 1:ncol(race.boolean)){
    successes = table(race.boolean[,i])["TRUE"]
    
    print(binom.test(as.integer(successes), l, demodata[,i]))
  }
}

BooleanLouisville(LouisvilleDemographics, LouisvilleCitations$subject_race_boolean)

