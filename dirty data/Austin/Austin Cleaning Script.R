#calling libraries
library(dplyr)
library(tidyr)
library(gtools)

#set working directory
setwd("C:/Users/katie/Desktop/policing/dirty data/Austin")

#Call datasets Austin (citations, use of force)
UOF15<-read.csv(file='R2R_2015.csv', stringsAsFactors = FALSE)
UOF14<-read.csv(file='R2R_2014.csv', stringsAsFactors = FALSE)
UOF16<-read.csv(file='R2R_2016.csv', stringsAsFactors = FALSE)
UOF17<-read.csv(file='R2R_2017.csv', stringsAsFactors = FALSE)
UOF18<-read.csv(file='R2R_2018.csv', stringsAsFactors = FALSE)
citations15<-read.csv(file='2015_RP__Citations.csv', stringsAsFactors = FALSE)
citations14<-read.csv(file='2014_RP__Citations.csv', stringsAsFactors = FALSE)
citations16<-read.csv(file='2016_RP__Citations.csv', stringsAsFactors = FALSE)
citations17<-read.csv(file='2017_RP__Citations.csv', stringsAsFactors = FALSE)
Citations18<-read.csv(file='2018_RP__Citations.csv', stringsAsFactors = FALSE)
Shooting_Incidents<-read.csv(file='Officer_Involved_Shootings_2008-17_Incidents.csv', stringsAsFactors = FALSE)
Shooting_officers<-read.csv(file='Officer_Involved_Shootings_2008-17_Officers.csv', stringsAsFactor = FALSE)
Shooting_Subjects<-read.csv(file='Officer_Involved_Shootings_2008-17_Subjects.csv', stringsAsFactors = FALSE)


#making columns the same so they merge easily
UOF14_Fix_Col<-subset(UOF14, select=-c(Subject.Effects))
UOF15_Fix_COl<-subset(UOF15, select=-c(Subject.Effects))
UOF16_Fix_Col<-subset(UOF16, select=-c(Subject.Effects))
UOF17_Fix_Col<-UOF17[, -c(29:32)]
UOF18_Fix_Col<-UOF18[, -c(29:32)]

#changing column names to match
UOF16_Fix_Col<-UOF16_Fix_Col %>% rename(Date.Occurred = Date..Occurred,
                                        Time.Occurred = Time..Occurred,
                                        Area.Command = Area..Command,
                                        Subject.Sex = Subject..Sex,
                                        Subject.Race = Subject..Race,
                                        Subject.Ethnicity = Subject..Ethnicity,
                                        Number.Shots = Number..Shots,
                                        Officer.Organization.Desc = Officer..Organization.Desc,
                                        Officer.Commission.Date = Officer..Commission.Date)

UOF17_Fix_Col<-UOF17_Fix_Col %>% rename(Date.Occurred = Date..Occurred,
                         Time.Occurred = Time..Occurred,
                         Area.Command = Area..Command,
                         Subject.Sex = Subject..Sex,
                         Subject.Race = Subject..Race,
                         Subject.Ethnicity = Subject..Ethnicity,
                         Number.Shots = Number..Shots,
                         Officer.Organization.Desc = Officer..Organization.Desc,
                         Officer.Commission.Date = Officer..Commission.Date)


UOF18_Fix_Col<-UOF18_Fix_Col%>% rename(Date.Occurred = Date..Occurred,
                        Time.Occurred = Time..Occurred,
                        Area.Command = Area..Command,
                        Subject.Sex = Subject..Sex,
                        Subject.Race = Subject..Race,
                        Subject.Ethnicity = Subject..Ethnicity,
                        Number.Shots = Number..Shots,
                        Officer.Organization.Desc = Officer..Organization.Desc,
                        Officer.Commission.Date = Officer..Commission.Date)


#bind them all together
UOF_ALL<-smartbind(UOF14_Fix_Col,UOF15_Fix_COl,UOF16_Fix_Col,UOF17_Fix_Col,UOF18_Fix_Col)


#numerize the data
#splitting year and date into  different variables
SplitDateTime_UOF_ALL<-strsplit(as.character(UOF_ALL$Date.Occurred), "\\s")
SplitDateTime_UOF_ALL<-do.call(rbind, SplitDateTime_UOF_ALL)
colnames(SplitDateTime_UOF_ALL)<-(c("date", "time"))
SplitDateTime_UOF_ALL<-as.data.frame(SplitDateTime_UOF_ALL, stringsAsFactors=FALSE)
#making a tables with relevant metadata
ALLMetadata_UOF<-cbind.data.frame(SplitDateTime_UOF_ALL$date,UOF_ALL$Time.Occurred,UOF_ALL$Subject.Race,UOF_ALL$Subject.Ethnicity,UOF_ALL$Subject.Sex,UOF_ALL$Officer.Yrs.of.Service,UOF_ALL$Area.Command, stringsAsFactors=FALSE)
colnames(ALLMetadata_UOF)<-(c("date","time","subject race","Subject Ethnicity","subject sex","officer Yrs of service","Area Command"))

#make all null values = NA for UOF
ALLMetadata_UOF_NA<-ALLMetadata_UOF
ALLMetadata_UOF_NA[ALLMetadata_UOF=="U"]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF==""]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF=="-1"]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF=="88"]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF=="-2"]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF=="-"]<-NA

#changing race to be words instead of single letters & to match Citations
AllMetadata_UOF_FixRace<-ALLMetadata_UOF_NA
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="W"]<-("White")
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="B"]<-("Black")
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="A"]<-("Asian")
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="H"]<-("Hispanic")
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="I"]<-("Native American")
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="M"]<-("Middle Eastern")
AllMetadata_UOF_FixRace$`subject race`[AllMetadata_UOF_FixRace$`subject race`=="P"]<-("Hawaiian or Pacific Islander")

#Saving new dataset into clean data
write.csv(AllMetadata_UOF_FixRace,"C:/Users/katie/Desktop/policing/clean data/Austin\\UseOfForce_Austin.csv",row.names = FALSE)

#fixing column names to match for easy combining
Citations18<-Citations18%>% rename(OFF.FROM.DATE = OffenseDate,
                                   CITATION.NUMBER = Citation.Number,
                                     OFF.TIME = OffenseTime,
                                     RACE.ORIGIN.CODE = Race,
                                     CASE.PARTY.SEX = Sex,
                                     RACE.KNOWN = Race_Known,)


#combining all citations data
Citations_ALL<-smartbind(citations14,citations15,citations16,citations17,Citations18)


#separating date and time in citations to get rid of unnecessary 00:00
SplitDateTime_Citations<-strsplit(as.character(Citations_ALL$OFF.FROM.DATE), "\\s")
SplitDateTime_Citations<-do.call(rbind, SplitDateTime_Citations)
colnames(SplitDateTime_Citations)<-(c("date","time"))
SplitDateTime_Citations<-cbind.data.frame(SplitDateTime_Citations, stringsAsFactors=FALSE)

#making a table with all relevant metadata for citations
AllMetadata_Citations<-cbind.data.frame(Citations_ALL$CITATION.NUMBER,SplitDateTime_Citations$date,Citations_ALL$OFF.TIME,Citations_ALL$RACE.ORIGIN.CODE,Citations_ALL$CASE.PARTY.SEX, stringsAsFactors=FALSE)
colnames(AllMetadata_Citations)<-(c("Citation Number","Date","Time","Subject Race","Subject gender"))

#make all empty values = NA
ALLMetadata_Citations_NA<-AllMetadata_Citations
ALLMetadata_Citations_NA[AllMetadata_Citations==""]<-NA
ALLMetadata_Citations_NA[AllMetadata_Citations=="O"]<-NA
ALLMetadata_Citations_NA[AllMetadata_Citations=="U"]<-NA

#changing race to be words
AllMetadata_citations_FixRace<-ALLMetadata_Citations_NA
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="W"]<-("White")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="Whi"]<-("White")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="WHI"]<-("White")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="B"]<-("Black")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="A"]<-("Asian")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="H"]<-("Hispanic")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="ME"]<-("Middle Eastern")
AllMetadata_citations_FixRace$`Subject Race`[AllMetadata_citations_FixRace$`Subject Race`=="N"]<-("Native American")

#saving new dataset to clean data file
write.csv(AllMetadata_citations_FixRace,"C:/Users/katie/Desktop/policing/clean data/Austin\\Citations_Austin.csv",row.names = FALSE)

#combining shootings into one database with all info together
OIS_ALL<-merge(Shooting_officers,Shooting_Subjects)
OIS_ALL<-merge(OIS_ALL,Shooting_Incidents)

#making a column all lower case to be consistent
OIS_ALL$Less.Lethal.Force.used.by.APD.prior.to.shooting.[OIS_ALL$Less.Lethal.Force.used.by.APD.prior.to.shooting.=="No"]<-("no")

#simplifying weapons columns
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="rifle"]<-("gun")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="rifle and shotgun"]<-("gun")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="handgun"]<-("gun")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="rifle and shotgun"]<-("gun")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="knife (machete)"]<-("knife")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="none (radio face plate)"]<-("none")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="air pistol (BB gun)"]<-("air pistol/pellet gun")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="air pistol (BB gun)"]<-("air pistol/pellet gun")
OIS_ALL$Subject.Weapon[OIS_ALL$Subject.Weapon=="pellet gun"]<-("air pistol/pellet gun")


#make a table with available data
AllMetadata_Shootings<-cbind.data.frame(OIS_ALL$Case..,OIS_ALL$Date,OIS_ALL$Time,OIS_ALL$Officer.Name,OIS_ALL$Rank,OIS_ALL$Officer.Race.Ethnicity,OIS_ALL$Officer.Gender,OIS_ALL$Officer.Age,OIS_ALL$Prev.OIS,OIS_ALL$Less.Lethal.Force.used.by.APD.prior.to.shooting.,
                                        OIS_ALL$Subject.Race.Ethnicity,OIS_ALL$Subject.Gender,OIS_ALL$Subject.Injuries,OIS_ALL$Subject.Weapon, stringsAsFactors = FALSE)
colnames(AllMetadata_Shootings)<-(c("Case","Date","Time","Officer Name","Officer Rank","Officer Race","Officer Gender","Officer Age","Officer Prev. OIS","Less than Lethal Forced Used by APD Prior to Shooting","Subject Race","Subject Gender","Subject Injuries","Subject Weapons" ))

#saving new dataset to clean data file
write.csv(AllMetadata_Shootings,"C:/Users/katie/Desktop/policing/clean data/Austin\\Shootings_Austin.csv",row.names = FALSE)
