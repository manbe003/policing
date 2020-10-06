#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

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
