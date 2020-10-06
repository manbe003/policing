#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

#I want to call in my datasets (shootings).
shootings<-read.csv(file='shootings_Seattle.csv', stringsAsFactors = TRUE)

#summary data
summary(shootings)
#only numerical columns are subject age and years of SPD service

var(shootings$Subject.Age, na.rm=TRUE)
sd(shootings$Subject.Age, na.rm=TRUE)

var(shootings$Years.of.SPD.Service, na.rm=TRUE)
sd(shootings$Years.of.SPD.Service, na.rm=TRUE)

#histogram of age
hist(shootings$Subject.Age, breaks = 25)

#boxplot
boxplot(shootings$Subject.Age)

#is age normally distributed
ggplot(shootings, aes(sample=Subject.Age)) + 
  geom_qq () +
  geom_qq_line()
#looks fairly normal except for a couple of outliers. Most are old people

#histogram of years of SPD
hist(shootings$Years.of.SPD.Service, breaks = 25)

#boxplot
boxplot(shootings$Years.of.SPD.Service)

#what about normality of years of SPD service
ggplot(shootings, aes(sample=Years.of.SPD.Service)) + 
  geom_qq () +
  geom_qq_line()
#looks more exponential to me. 

#categorical data analysis
levels(shootings$Officer.Gender)
levels(shootings$Officer.Race)
levels(shootings$Officer.Injured)
levels(shootings$Number.of.Rounds)
levels(shootings$Subject.Gender)
levels(shootings$Subject.Race)
levels(shootings$Subject.Weapon)
levels(shootings$Type.of.Weapon)
levels(shootings$Fatal)
levels(shootings$On.duty)
levels(shootings$Justified)
levels(shootings$Within.Policy)
levels(shootings$Officer.Disciplined.)
#the ones that return NULL are T/F

ggplot(shootings, aes(Officer.Gender))+
  geom_bar()

ggplot(shootings, aes(Officer.Race))+
  geom_bar()

ggplot(shootings, aes(Officer.Injured))+
  geom_bar()

ggplot(shootings, aes(Number.of.Rounds))+
  geom_bar()

ggplot(shootings, aes(Subject.Gender))+
  geom_bar()

ggplot(shootings, aes(Subject.Race))+
  geom_bar()

ggplot(shootings, aes(Subject.Weapon))+
  geom_bar()

ggplot(shootings, aes(Type.of.Weapon))+
  geom_bar()

ggplot(shootings, aes(Fatal))+
  geom_bar()

ggplot(shootings, aes(On.duty))+
  geom_bar()

ggplot(shootings, aes(Justified))+
  geom_bar()

ggplot(shootings, aes(Within.Policy))+
  geom_bar()

ggplot(shootings, aes(Officer.Disciplined.))+
  geom_bar()


ggplot(shootings,
       aes(x = Subject.Race,
           fill = Within.Policy))+
  geom_bar(position = "dodge")

ggplot(shootings,
       aes(x = Type.of.Weapon,
           fill = Subject.Race))+
  geom_bar(position = "dodge")

ggplot(shootings,
       aes(x = Subject.Race,
           fill = Subject.Weapon))+
  geom_bar(position = "dodge")
