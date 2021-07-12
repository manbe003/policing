#libraries
library(here)
library(tidyverse)
library(epitools)
library(tidyr)
library(dplyr)

#loading datasets
UOF <- read.csv(file = here("clean data/Austin/UseOfForce_Austin.csv"), stringsAsFactors = FALSE)
Shootings<- read.csv(file = here("clean data/Austin/Shootings_Austin.csv"), stringsAsFactors = FALSE)

#making it so 1 is the worse outcome and 2 or 3 is the better, and anything not 1,2,or3 to NA because I cant find what they mean
UOF1<-UOF
UOF1$R2R.Level<- gsub('0|13|14|23|24|34|4',NA,UOF1$R2R.Level)
UOF1$R2R.Level<- gsub('1',"Lethal" ,UOF1$R2R.Level)
UOF1$R2R.Level<- gsub('2|3',"Less Than Lethal" ,UOF1$R2R.Level)
#taking out these races because they have 0 counts in "worse" so it wont compute an OR
UOF1$subject.race<- gsub('Asian|Hawaiian or Pacific Islander|Middle Eastern|Native American',NA ,UOF1$subject.race)

#dataframe of variables I want
VictimRace_UOFLevel <- as.data.frame(na.omit(cbind(as.character(UOF1$R2R.Level), as.character(UOF1$subject.race))))

#making a table of Force levels and Race counts
Level.Race<-table(VictimRace_UOFLevel$V2, VictimRace_UOFLevel$V1)
Level.Race <- Level.Race[c(2,1),]
print(Level.Race)

#OR
Race_OR<-oddsratio(Level.Race)
#printing the outcome so its easier to read
print(Race_OR$measure)
print(Race_OR$p.value)

#graph
ggplot(VictimRace_UOFLevel,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")



##now I'm making it so 3 is better outcome and 2/1 is worse
UOF2<-UOF
UOF2$R2R.Level<- gsub('0|13|14|23|24|34|4',NA,UOF2$R2R.Level)
UOF2$R2R.Level<- gsub('1|2',"Worse" ,UOF2$R2R.Level)
UOF2$R2R.Level<- gsub('3',"Better" ,UOF2$R2R.Level)
UOF2$subject.race<- gsub('Hawaiian or Pacific Islander|Native American',NA ,UOF2$subject.race)

#dataframe of variables I want
VictimRace_UOFLevel2 <- as.data.frame(na.omit(cbind(as.character(UOF2$R2R.Level), as.character(UOF2$subject.race))))

#making a table of Force levels and Race counts
Level.Race2<-table(VictimRace_UOFLevel2$V2, VictimRace_UOFLevel2$V1)
Level.Race2 <- Level.Race2[c(4,1:3),]
print(Level.Race2)

#OR
Race_OR2<-oddsratio(Level.Race2)
#printing the outcome so its easier to read
print(Race_OR2$measure)
print(Race_OR2$p.value)

#graph
ggplot(VictimRace_UOFLevel2,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")

##### OR for shootings

#dataframe of wanted variables

Shootings<- read.csv(file = here("clean data/Austin/Shootings_Austin.csv"), stringsAsFactors = FALSE)
Shootings2<- Shootings
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="killed (self-inflicted)"]<-(NA)
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="shot (injured only)"]<-('Injured/no Injury')
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="injured - incidentally"]<-('Injured/no Injury')
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="None"]<-('Injured/no Injury')

Shooting_Outcome <-as.data.frame(na.omit(cbind(as.character(Shootings2$Subject.Injuries), as.character(Shootings2$Subject.Race))))


#making a table of Fatality and Race counts
Fatality.Race<-table(Shooting_Outcome$V2, Shooting_Outcome$V1)
Fatality.Race <- Fatality.Race[c(3,1:2),]
print(Fatality.Race)

Fatality_OR<-oddsratio(Fatality.Race)
#printing the outcome so its easier to read
print(Fatality_OR$measure)
print(Fatality_OR$p.value)

ggplot(Shooting_Outcome,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")



