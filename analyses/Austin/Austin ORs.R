#load dependencies and set working directory
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()
setwd(here())

#loading data sets
UOF <- read.csv(file = here("clean data/Austin/UseOfForce_Austin.csv"), stringsAsFactors = FALSE)
Shootings<- read.csv(file = here("clean data/Austin/Shootings_Austin.csv"), stringsAsFactors = FALSE)

## ORs for UOF ##
#*Austin R2R does not map on directly to our coding of UOF (1- lethal weapon, 2- non lethal weapon,3- no weapon)*

##This is the equivalent to our Lethal vs Non Lethal odds ratio
#Key to Austin R2R 1- Lethal, 2- less than lethal, 3- minor force
#Binning it so lvl 1 is "lethal" and 2 & 3 is "Less than Lethal", and anything other than that to NA
UOF1<-UOF
UOF1$R2R.Level<- gsub('0|13|14|23|24|34|4',NA,UOF1$R2R.Level)
UOF1$R2R.Level<- gsub('1',"Lethal" ,UOF1$R2R.Level)
UOF1$R2R.Level<- gsub('2|3',"Less Than Lethal" ,UOF1$R2R.Level)

#taking out these races because they have 0 counts in the column Lethal so it wont compute an OR
UOF1$subject.race<- gsub('Asian|Hawaiian or Pacific Islander|Middle Eastern|Native American',NA ,UOF1$subject.race)

#data frame of variables I want
VictimRace_UOFLevel <- as.data.frame(na.omit(cbind(as.character(UOF1$R2R.Level), as.character(UOF1$subject.race))))

#making a table of Force levels and Race counts
Level.Race<-table(VictimRace_UOFLevel$V2, VictimRace_UOFLevel$V1)
Level.Race <- Level.Race[c(2,1),]
Level.Race <- Level.Race[ , c("Lethal", "Less Than Lethal")]
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



##This is the equivalent to our Weapon Vs. No Weapon odds Ratio
#Key to Austin R2R 1- Lethal, 2- less than lethal, 3- minor force
#Binning lvls 1 & 2 to "higher force" and lvl 3 as "minor force", anything else to NA
UOF2<-UOF
UOF2$R2R.Level<- gsub('0|13|14|23|24|34|4',NA,UOF2$R2R.Level)
UOF2$R2R.Level<- gsub('1|2',"Higher Force" ,UOF2$R2R.Level)
UOF2$R2R.Level<- gsub('3',"Minor Force" ,UOF2$R2R.Level)
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


## OR for shootings ##

#Changing variables to Injured/No Injury vs Killed by PD, anything else NA
Shootings2<- Shootings 
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="killed (self-inflicted)"]<-(NA)
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="shot (injured only)"]<-('Injured/no Injury')
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="injured - incidentally"]<-('Injured/no Injury')
Shootings2$Subject.Injuries[Shootings2$Subject.Injuries=="None"]<-('Injured/no Injury')

Shooting_Outcome <-as.data.frame(na.omit(cbind(as.character(Shootings2$Subject.Injuries), as.character(Shootings2$Subject.Race))))


#making a table of Fatality and Race counts
Fatality.Race<-table(Shooting_Outcome$V2, Shooting_Outcome$V1)
Fatality.Race <- Fatality.Race[c(3,1:2),]
Fatality.Race <- Fatality.Race[ , c("killed", "Injured/no Injury")]
print(Fatality.Race)

Fatality_OR<-oddsratio(Fatality.Race)
#printing the outcome so its easier to read
print(Fatality_OR$measure)
print(Fatality_OR$p.value)

ggplot(Shooting_Outcome,
       aes(x = V1,
           fill = V2))+
  geom_bar(position = "dodge")



