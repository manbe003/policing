#libraries
library(here)
library(tidyverse)
library(epitools)
library(tidyr)
library(dplyr)

#load in datasets
UOF<-  read.csv(file = here("clean data/seattle/UseofForce_Seattle.csv"), stringsAsFactors = FALSE)
Shootings<- read.csv(file = here("clean data/seattle/shootings_Seattle.csv"), stringsAsFactors = FALSE)


#comparing UOF 2 or less to 3 (less than lethal vs lethal/serious injury)
UOF1<- UOF
UOF1$Incident_Type<- gsub('Level 2 - Use of Force|Level 1 - Use of Force',"Less Than Lethal" ,UOF1$Incident_Type)
UOF1$Incident_Type<- gsub('Level 3 - Use of Force|Level 3 - OIS',"Serious Injury/Lethal Force" ,UOF1$Incident_Type)
Race_Type <- as.data.frame(na.omit(cbind(as.character(UOF1$Incident_Type), as.character(UOF1$Subject_Race))))

#table
Level.Race<-table(Race_Type$V2, Race_Type$V1)
Level.Race <- Level.Race[c(6,1:5),]
print(Level.Race)

#OR
Race_OR<-oddsratio(Level.Race)
#printing the outcome so its easier to read
print(Race_OR$measure)
print(Race_OR$p.value)

ggplot(Race_Type,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")


##comparing UOF level 1 to 2/3
UOF2<- UOF
UOF2$Incident_Type<- gsub('Level 1 - Use of Force',"minor pain" ,UOF2$Incident_Type)
UOF2$Incident_Type<- gsub('Level 3 - Use of Force|Level 3 - OIS|Level 2 - Use of Force',"less than leathal/Lethal Force" ,UOF2$Incident_Type)
Race_Type2 <- as.data.frame(na.omit(cbind(as.character(UOF2$Incident_Type), as.character(UOF2$Subject_Race))))

Level.Race2<-table(Race_Type2$V2, Race_Type2$V1)
Level.Race2 <- Level.Race2[c(6,1:5),]
print(Level.Race2)

#OR
Race_OR2<-oddsratio(Level.Race2)
#printing the outcome so its easier to read
print(Race_OR2$measure)
print(Race_OR2$p.value)

ggplot(Race_Type2,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")


###Shootings
Outcome<- as.data.frame(na.omit(cbind(as.character(Shootings$Fatal), as.character(Shootings$Subject.Race))))
Outcome$V2<- gsub('American Indian or Alaska Native|Hispanic', NA ,Outcome$V2)

Race.Fatal<-table(Outcome$V2, Outcome$V1)
Race.Fatal <- Race.Fatal[c(3,1:2),]
print(Race.Fatal)

Shooting_OR<-oddsratio(Race.Fatal)
#printing the outcome so its easier to read
print(Shooting_OR$measure)
print(Shooting_OR$p.value)

