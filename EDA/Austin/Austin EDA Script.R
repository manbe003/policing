#Austin EDA

#load library
library(tidyverse)
library(here)

#Descriptive data
UOF_EDA<-read.csv(file=here('clean data/Austin/UseOfForce_Austin.csv'), stringsAsFactors = TRUE)
Shootings_EDA<-read.csv(file=here('clean data/Austin/Shootings_Austin.csv'), stringsAsFactors = TRUE)
Citations_EDA<-read.csv(file=here('clean data/Austin/Citations_Austin.csv'), stringsAsFactors = TRUE)

summary(UOF_EDA)
summary(Shootings_EDA)
summary(Citations_EDA)


#standard deviations
sd(Shootings_EDA$Officer.Age)
sd(UOF_EDA$officer.Yrs.of.service, na.rm=TRUE)


#variation
var(Shootings_EDA$Officer.Age)
var(UOF_EDA$officer.Yrs.of.service, na.rm=TRUE)

##graphs
hist(as.numeric(Shootings_EDA$Officer.Age), breaks = 50)
hist(as.numeric(UOF_EDA$officer.Yrs.of.service), breaks = 50)

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


##categorical data

##UOF
#levels
levels(UOF_EDA$subject.race)
levels(UOF_EDA$subject.sex) 
levels(UOF_EDA$Subject.Ethnicity)
levels(UOF_EDA$Area.Command)    

#graphs

ggplot(UOF_EDA, aes(subject.race)) +
  geom_bar()

ggplot(UOF_EDA, aes(subject.sex)) +
  geom_bar()

ggplot(UOF_EDA, aes(Subject.Ethnicity)) +
  geom_bar()

ggplot(UOF_EDA, aes(Area.Command)) +
  geom_bar()

#comparison graph
ggplot(UOF_EDA,
       aes(x = subject.race,
           fill = Area.Command))+
  geom_bar(position = "dodge")


##citations
#levels
levels(Citations_EDA$Subject.Race)
levels(Citations_EDA$Subject.gender)

#graphs

ggplot(Citations_EDA, aes(Subject.Race)) +
  geom_bar()

ggplot(Citations_EDA, aes(Subject.gender)) +
  geom_bar()

##shootings

#levels
levels(Shootings_EDA$Officer.Rank)
levels(Shootings_EDA$Officer.Race)
levels(Shootings_EDA$Officer.Gender)
levels(Shootings_EDA$Officer.Prev..OIS)
levels(Shootings_EDA$Less.than.Lethal.Forced.Used.by.APD.Prior.to.Shooting)
levels(Shootings_EDA$Subject.Race)
levels(Shootings_EDA$Subject.Gender)
levels(Shootings_EDA$Subject.Injuries)
levels(Shootings_EDA$Subject.Weapons)

#graphs

ggplot(Shootings_EDA, aes(Officer.Rank)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Officer.Race)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Officer.Gender)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Officer.Prev..OIS)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Less.than.Lethal.Forced.Used.by.APD.Prior.to.Shooting)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Subject.Race)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Subject.Gender)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Subject.Injuries)) +
  geom_bar()

ggplot(Shootings_EDA, aes(Subject.Weapons)) +
  geom_bar()

#comparison graphs
ggplot(Shootings_EDA,
       aes(x = Officer.Race,
           fill = Officer.Prev..OIS))+
  geom_bar(position = "dodge")


ggplot(Shootings_EDA,
       aes(x = Subject.Race,
           fill = Subject.Injuries))+
  geom_bar(position = "dodge")


ggplot(Shootings_EDA,
       aes(x = Subject.Race,
           fill = Less.than.Lethal.Forced.Used.by.APD.Prior.to.Shooting))+
  geom_bar(position = "dodge")


ggplot(Shootings_EDA,
       aes(x = Officer.Race,
           fill = Less.than.Lethal.Forced.Used.by.APD.Prior.to.Shooting))+
  geom_bar(position = "dodge")


ggplot(Shootings_EDA,
       aes(x = Subject.Race,
           fill = Subject.Weapons))+
  geom_bar(position = "dodge")


ggplot(Shootings_EDA,
       aes(x = Subject.Race,
           fill = Officer.Race))+
  geom_bar(position = "dodge")










       
       
       
       
       