#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

#I want to call in my datasets (citations).
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = TRUE)
#and now rule out all that are repeats of same suspect ID
UOF_sansrepeats<-distinct(UOF,UOF$Subject_ID, .keep_all=TRUE)

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



#shootings EDA
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




#citations EDA
#I want to call in my datasets (citations).
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = TRUE)

#summary data
summary(citations)

#only numeric is YOB
var(citations$Officer.YOB, na.rm=TRUE)
sd(citations$Officer.YOB, na.rm=TRUE)

#histogram of YOB
hist(citations$Officer.YOB, breaks = 25)

#boxplot
boxplot(citations$Officer.YOB)

#is age normally distributed
ggplot(citations, aes(sample=Officer.YOB)) + 
  geom_qq () +
  geom_qq_line()
#looks fairly normal except for a ceiling effect

#categorical data analysis
levels(citations$Subject.Age.Group)
levels(citations$Stop.Resolution)
levels(citations$Weapon.Type)
levels(citations$Officer.Gender)
levels(citations$Officer.Race)
levels(citations$Subject.Perceived.Race)
levels(citations$Subject.Perceived.Gender)
levels(citations$Initial.Call.Type)
levels(citations$Final.Call.Type)
levels(citations$Call.Type)
levels(citations$Arrest.Flag)
levels(citations$Frisk.Flag)

ggplot(citations, aes(Subject.Age.Group))+
  geom_bar()

ggplot(citations, aes(Stop.Resolution))+
  geom_bar()

ggplot(citations, aes(Weapon.Type))+
  geom_bar()

ggplot(citations, aes(Officer.Gender))+
  geom_bar()

ggplot(citations, aes(Officer.Race))+
  geom_bar()

ggplot(citations, aes(Subject.Perceived.Race))+
  geom_bar()

ggplot(citations, aes(Subject.Perceived.Gender))+
  geom_bar()

ggplot(citations, aes(Call.Type))+
  geom_bar()

ggplot(citations, aes(Arrest.Flag))+
  geom_bar()

ggplot(citations, aes(Frisk.Flag))+
  geom_bar()

ggplot(citations,
       aes(x = Subject.Perceived.Race,
           fill = Frisk.Flag))+
  geom_bar(position = "dodge")

