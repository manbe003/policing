#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)
library(Policingfunctions)
library(tidyverse)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = FALSE)
shootings<-read.csv(file='shootings_Seattle.csv', stringsAsFactors = FALSE)

#summary data
summary(shootings)
#only numerical columns are subject age and years of SPD service

var(shootings$Subject.Age, na.rm=TRUE)
sd(shootings$Subject.Age, na.rm=TRUE)

var(shootings$Years.of.SPD.Service, na.rm=TRUE)
sd(shootings$Years.of.SPD.Service, na.rm=TRUE)

#is age normally distributed
ggplot(shootings, aes(sample=Subject.Age)) + 
  geom_qq () +
  geom_qq_line()
#looks fairly normal except for a couple of outliers. Most are old people

#histogram of age
hist(shootings$Subject.Age, breaks = 25)

#what about normality of years of SPD service
ggplot(shootings, aes(sample=Years.of.SPD.Service)) + 
  geom_qq () +
  geom_qq_line()
#looks more exponential to me. 

#histogram of years of SPD
hist(shootings$Years.of.SPD.Service, breaks = 25)


#categorical data analysis
levels(as.factor(shootings$Officer.Gender))
levels(as.factor(shootings$Officer.Race))
levels(as.factor(shootings$Officer.Injured))
levels(as.factor(shootings$Number.of.Rounds))
levels(as.factor(shootings$Subject.Gender))
levels(as.factor(shootings$Subject.Race))
levels(as.factor(shootings$Subject.Weapon))
levels(as.factor(shootings$Type.of.Weapon))
levels(as.factor(shootings$Fatal))
levels(as.factor(shootings$On.duty))
levels(as.factor(shootings$Justified))
levels(as.factor(shootings$Within.Policy))
levels(as.factor(shootings$Officer.Disciplined.))

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
