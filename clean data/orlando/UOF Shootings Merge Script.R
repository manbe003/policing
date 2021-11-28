#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())

#loading in Datasets
UOF<- read.csv(file = "clean data/orlando/UOF (cleaned).csv")
Shootings <- read.csv(file = "clean data/orlando/shooting (cleaned).csv")           

#removing unnecessary columns and naming them the same to make the merge easy
UOF <- subset(UOF, select = -c(3,4,11))
names(UOF)[names(UOF) == 'Officers.Involved'] <- 'Number.Of.Officers.Involved'
names(UOF)[names(UOF) == 'Offenders.Race']<- 'Suspect.Race'
names(UOF)[names(UOF) == 'Offenders.Ethnicity']<- 'Suspect.Ethnicity'
names(UOF)[names(UOF) == 'Offenders.Sex']<- 'Suspect.Sex'


Shootings <- subset(Shootings, select = -c(4,10,11))
names(Shootings)[names(Shootings) == 'Officer.Race']<- 'Officers.Race'
names(Shootings)[names(Shootings) == 'Officer.Ethnicity']<- 'Officers.Ethnicity'
names(Shootings)[names(Shootings) == 'Officer.Gender']<- 'Officers.Sex'
names(Shootings)[names(Shootings) == 'Suspect.Gender']<- 'Suspect.Sex'


#putting in Force Level Column into Shootings and Officer number binning
Shootings$UOF.Level <- 3 #all 3 bc theyre all shootings
#Binning Number of Officers
Shootings['Binning.Number.of.Officers'] <- NA
Shootings$Binning.Number.of.Officers[Shootings$Number.Of.Officers=="1"]<- "1"
Shootings$Binning.Number.of.Officers[Shootings$Number.Of.Officers=="2"]<- "2"
Shootings$Binning.Number.of.Officers[Shootings$Number.Of.Officers > "2"]<- "3+"

#rbinding them
Combined <- rbind(UOF, Shootings)

#Saving this so it can be used for an OR
write.csv(Combined,(file=here('clean data/orlando/UOFShootingsCombined.csv')), row.names = FALSE)
