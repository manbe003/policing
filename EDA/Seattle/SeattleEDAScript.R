#Seattle EDA

setwd("C:/Users/dmcmanu1/policing/clean data/seattle")

library(tidyverse)

##Descriptive Data

UOF = read.csv(file="UseOfForce_Seattle.csv", stringsAsFactors = FALSE)
shootings = read.csv(file = "shootings_Seattle.csv", stringsAsFactors = FALSE)

summary(UOF)
summary(shootings)

x = shootings$Number.of.Rounds
y = as.numeric(x)
z = na.exclude(y)

sd(shootings$Subject.Age)
sd(z)

shootings$NARounds =  as.numeric(shootings$Number.of.Rounds)
RoundswoNAs = na.exclude(shootings$NARounds)
RoundswoNAs = as.data.frame(RoundswoNAs)

sd(RoundswoNAs)
var(z)
summary(RoundswoNAs)

###Graphs

hist(as.numeric(shootings$Years.of.SPD.Service), breaks = 50)
hist(as.numeric(shootings$Subject.Age), breaks = 50)

##Outliers

boxplot(as.numeric(shootings$Subject.Age))


##Normaliy

xyz = as.numeric(shootings$Years.of.SPD.Service)
xyz = as.data.frame(xyz)


ggplot(xyz, aes(sample = xyz)) + 
  geom_qq() +
  geom_qq_line()

  