#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)

#set working directory
setwd("/Users/katherine/Policing/dirty data/seattle")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='use_of_force_Seattle.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='citations_seattle.csv', stringsAsFactors = FALSE)
shootings<-read.csv(file='Officer_involved_shooting_Seattle.csv', stringsAsFactors = FALSE)

#numericize the data
#first split year/date into two separate variables
SplitDateTime_UOF<-strsplit(as.character(UOF$Occured_date_time),"\\s")
SplitDateTime_UOF<-do.call(rbind, SplitDateTime_UOF)
SplitDateTime_UOF<-as.data.frame(SplitDateTime_UOF, stringsAsFactors=FALSE)
colnames(SplitDateTime_UOF)<-(c("date","time"))

#making a table with all relevant metadata for UOF
AllMetadata_UOF<-cbind.data.frame(SplitDateTime_UOF[,1:2],UOF[,1:3],UOF[,5:11], stringsAsFactors=FALSE)

#look at the table
View(AllMetadata_UOF)

#make all null values = NA for UOF
AllMetadata_UOF_NA<-AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF=="Not Specified"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="-"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="X"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="2300 BLOCK CALIFORNIA AV SW"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="2700 BLOCK UTAH AV S"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="4200 BLOCK E MADISON ST"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="500 BLOCK S MAIN ST"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="5200 BLOCK UTAH AV S"]<-NA

#Change race to be consisted with citations
AllMetadata_UOF_FixRace<-AllMetadata_UOF_NA
AllMetadata_UOF_FixRace[AllMetadata_UOF=="American Indian/Alaska Native"]<-"American Indian or Alaska Native"
AllMetadata_UOF_FixRace[AllMetadata_UOF=="Nat Hawaiian/Oth Pac Islander"]<-"Native Hawaiian or Other Pacific Islander"
AllMetadata_UOF_FixRace[AllMetadata_UOF=="Hispanic or Latino"]<-"Hispanic"

#make yes/no = true/false
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="yes"]<-TRUE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="Yes"]<-TRUE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="YES"]<-TRUE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="no"]<-FALSE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="No"]<-FALSE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="NO"]<-FALSE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="N"]<-FALSE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="Y"]<-TRUE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="n"]<-FALSE
AllMetadata_UOF_FixRace[AllMetadata_UOF_FixRace=="y"]<-TRUE

#There are two of each letter in sector. Here I'm trying to make all the letters the same. 
AllMetadata_UOF_FixSector<-AllMetadata_UOF_FixRace
AllMetadata_UOF_FixSector<-as.data.frame(substr(AllMetadata_UOF_NA$Sector, 0,1),stringsAsFactors = FALSE)
colnames(AllMetadata_UOF_FixSector)<-c("sector")
AllMetadata_UOF_Standardized<-cbind.data.frame(AllMetadata_UOF_FixRace[,1:6],AllMetadata_UOF_FixSector[,1], AllMetadata_UOF_FixRace[,8:12])
colnames(AllMetadata_UOF_Standardized)[7]<-c("sector")

#okay, now we'll export into a new dataset in a clean data folder
write.csv(AllMetadata_UOF_Standardized,"/Users/katherine/Policing/clean data/seattle/UseOfForce_Seattle.csv",row.names = FALSE)


#need to reorder date for citations to follow m/d/y format
FixDate_Citations<-strsplit(as.character(citations$Reported.Date),"T")
FixDate_Citations<-do.call(rbind, FixDate_Citations)
FixDate_Citations<-FixDate_Citations[,1]
FixDate_Citations<-strsplit(as.character(FixDate_Citations),"-")
FixDate_Citations<-do.call(rbind, FixDate_Citations)
FixDate_Citations<-FixDate_Citations[,c(2,3,1)]
colnames(FixDate_Citations)<-(c("1","2","3"))
FixDate_Citations<-as.data.frame(FixDate_Citations)
FixDate_Citations<-unite(FixDate_Citations,year,sep="/")
colnames(FixDate_Citations)<-c("date")
FixTime_Citations<-strsplit(as.character(citations$Reported.Time),":")
FixTime_Citations<-do.call(rbind,FixTime_Citations)
FixTime_Citations<-as.data.frame(FixTime_Citations)
FixTime_Citations<-unite(FixTime_Citations,FixTime_Citations,(V1:V2),sep=":")
colnames(FixTime_Citations)<-c("time")

#make all null/error values = NA for citations
AllMetadata_Citations_NA<-citations
AllMetadata_Citations_NA[citations=="FK ERROR"]<-NA
AllMetadata_Citations_NA[citations=="OOJ"]<-NA
AllMetadata_Citations_NA[citations=="Unknown"]<-NA
AllMetadata_Citations_NA[citations=="Other"]<-NA
AllMetadata_Citations_NA[citations=="Unable to Determine"]<-NA
AllMetadata_Citations_NA[citations=="Not Specified"]<-NA
AllMetadata_Citations_NA[citations=="-"]<-NA

#edit officer race for consistency
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="American Indian/Alaska Native"]<-"American Indian or Alaska Native"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Nat Hawaiian/Oth Pac Islander"]<-"Native Hawaiian or Other Pacific Islander"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Hispanic or Latino"]<-"Hispanic"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Two or More Races"]<-"Multi-Racial"

#make yes/no = true/false
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="yes"]<-TRUE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Yes"]<-TRUE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="YES"]<-TRUE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="no"]<-FALSE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="No"]<-FALSE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="NO"]<-FALSE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Y"]<-TRUE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="y"]<-TRUE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="n"]<-FALSE
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="N"]<-FALSE

#fix weapon type for consistency with shootings
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Firearm (unk type)"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Knife/Cutting/Stabbing Instrument"]<-"Knife"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Rifle"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Automatic Handgun"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Firearm"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Handgun"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Firearm Other"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Shotgun"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Other Firearm"]<-"gun"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Lethal Cutting Instrument"]<-"Knife"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="None/Not Applicable"]<-"None"
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Club"]<-"Blunt Object/Striking Implement" 
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Blackjack"]<-"Blunt Object/Striking Implement" 
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Brass Knuckles"]<-"Blunt Object/Striking Implement" 
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="Club, Blackjack, Brass Knuckles"]<-"Blunt Object/Striking Implement" 


#formatting of 99 and sector is weird. Fix by only taking the first value.
AllMetadata_Citations_NA$Sector<-substr(AllMetadata_Citations_NA$Sector,0,1)

#Back to NA replace
AllMetadata_Citations_NA[AllMetadata_Citations_NA=="9"]<-NA


#sometimes spelled Southwest, sometimes SouthWest
AllMetadata_Citations_CleanPrecinct<-AllMetadata_Citations_NA
AllMetadata_Citations_CleanPrecinct[AllMetadata_Citations_CleanPrecinct=="SouthWest"]<-"Southwest"

#Make dataset with all updated data
AllMetadata_Citations_NA$Reported.Date<-FixDate_Citations$date
AllMetadata_Citations_NA$Reported.Time<-FixTime_Citations[,1]
AllMetadata_Citations_NA$Precinct<-AllMetadata_Citations_CleanPrecinct$Precinct

#officer ID has two spaces after it. Need to be cleaned
AllMetadata_Citations_NA$Officer.ID <-trimws(citations$Officer.ID)

#write the file to a new location in clean data folder!
write.csv(AllMetadata_Citations_NA,"/Users/katherine/Policing/clean data/seattle/citations_Seattle.csv",row.names = FALSE)


#finally fix the officer involved shooting dataset. I need to standardize times and races
AllMetadata_shootings<-shootings
AllMetadata_shootings[AllMetadata_shootings=="American Indian/Alaska Native"]<-("American Indian or Alaska Native")
AllMetadata_shootings[AllMetadata_shootings=="Nat Hawaiian/Oth Pac Islander"]<-("Native Hawaiian or Other Pacific Islander")
AllMetadata_shootings[AllMetadata_shootings=="Hispanic or Latino"]<-"Hispanic"
AllMetadata_shootings[AllMetadata_shootings=="Hispanic/Latino"]<-"Hispanic"
AllMetadata_shootings[AllMetadata_shootings=="AI/AN"]<-"American Indian or Alaska Native"
AllMetadata_shootings[AllMetadata_shootings=="Asian/Pacific Islander"]<-"Asian"
AllMetadata_shootings[AllMetadata_shootings=="Black"]<-"Black or African American"
AllMetadata_shootings[AllMetadata_shootings=="Black "]<-"Black or African American"
AllMetadata_shootings[AllMetadata_shootings=="\nBlack"]<-"Black or African American"
AllMetadata_shootings[AllMetadata_shootings=="Native American"]<-"American Indian or Alaska Native"
AllMetadata_shootings[AllMetadata_shootings=="\nMale"]<-"Male"
AllMetadata_shootings[AllMetadata_shootings=="\nYes"]<-"Yes"
AllMetadata_shootings[AllMetadata_shootings=="\nNo"]<-"No"
AllMetadata_shootings[AllMetadata_shootings=="On"]<-"No"
AllMetadata_shootings[AllMetadata_shootings=="Within Policy "]<-"Justified"
AllMetadata_shootings[AllMetadata_shootings=="Within Policy"]<-"Justified"
AllMetadata_shootings[AllMetadata_shootings=="Out of Policy"]<-"Not Justified"

#add a colon 2 spaces into time
shooting_hour<-as.numeric(substr(AllMetadata_shootings$Time,1,nchar(AllMetadata_shootings$Time)-2))
shooting_minute<-as.numeric(substr(AllMetadata_shootings$Time,nchar(AllMetadata_shootings$Time)-1,nchar(AllMetadata_shootings$Time)))
shooting_time<-as.data.frame(cbind(shooting_hour,shooting_minute))

#add 0s to time
shooting_time$shooting_hour<-str_pad(shooting_time$shooting_hour, 2, pad = "0")
shooting_time$shooting_minute<-str_pad(shooting_time$shooting_minute, 2, pad = "0")
shooting_time<-unite(shooting_time,time,sep=":")

#now make all missings = NA
AllMetadata_shootings[AllMetadata_shootings=="MISSING"]<-NA
AllMetadata_shootings[AllMetadata_shootings==""]<-NA
AllMetadata_shootings[AllMetadata_shootings=="\nUnknown"]<-NA
AllMetadata_shootings[AllMetadata_shootings=="Missing"]<-NA

#make yes/no = true/false
AllMetadata_shootings[AllMetadata_shootings=="yes"]<-TRUE
AllMetadata_shootings[AllMetadata_shootings=="Yes"]<-TRUE
AllMetadata_shootings[AllMetadata_shootings=="YES"]<-TRUE
AllMetadata_shootings[AllMetadata_shootings=="no"]<-FALSE
AllMetadata_shootings[AllMetadata_shootings=="No"]<-FALSE
AllMetadata_shootings[AllMetadata_shootings=="NO"]<-FALSE
AllMetadata_shootings[AllMetadata_shootings=="Y"]<-TRUE
AllMetadata_shootings[AllMetadata_shootings=="y"]<-TRUE
AllMetadata_shootings[AllMetadata_shootings=="n"]<-FALSE
AllMetadata_shootings[AllMetadata_shootings=="N"]<-FALSE

#clean weapon type
AllMetadata_shootings[AllMetadata_shootings=="\n9mm semi-automatic"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings==".22 caliber pistol"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings==".357 revolver"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="6 shot .357 revolver"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Colt Revolver"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Gun"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Mac-10, 9 mm machine pistol"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Rifle"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Rifle   "]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Rifle w/ bayonet"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Semil automatic .38 caliber handgun"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Handgun"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="Shotgun"]<-"gun"
AllMetadata_shootings[AllMetadata_shootings=="N/A"]<-NA
AllMetadata_shootings[AllMetadata_shootings=="No "]<-"No"

#make years of SPD service numeric
AllMetadata_shootings[AllMetadata_shootings=="<1"]<-0
AllMetadata_shootings$Years.of.SPD.Service<-as.numeric(AllMetadata_shootings$Years.of.SPD.Service)

#clean number of rounds
AllMetadata_shootings$Number.of.Rounds[AllMetadata_shootings$Number.of.Rounds=="9"]<-"Multiple"
AllMetadata_shootings$Number.of.Rounds[AllMetadata_shootings$Number.of.Rounds=="14"]<-"Multiple"

#combine data into one dataset
AllMetadata_shootings_clean<-cbind.data.frame(AllMetadata_shootings[,1:3],shooting_time,AllMetadata_shootings[,5:28])

#save!
write.csv(AllMetadata_shootings_clean,"/Users/katherine/Policing/clean data/seattle/shootings_Seattle.csv",row.names = FALSE)
