#Bloomington Cleaning Script

#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
PackageDependency()

install.packages("hablar")
library(hablar)
install.packages("sqldf")
library(sqldf)

#load files
BloomUOF2016First<-read.csv(file = 'dirty data/Bloomington/2016 First Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2016Second<-read.csv(file = 'dirty data/Bloomington/2016 Second Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2016Third<-read.csv(file = 'dirty data/Bloomington/2016 Third Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2016Fourth<-read.csv(file = 'dirty data/Bloomington/2016 Fourth Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)

BloomUOF2017First<-read.csv(file = 'dirty data/Bloomington/2017 First Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2017Second<-read.csv(file = 'dirty data/Bloomington/2017 Second Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2017Third<-read.csv(file = 'dirty data/Bloomington/2017-third-quarter-use-of-force.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2017Fourth<-read.csv(file = 'dirty data/Bloomington/2017-fourth-quarter-use-of-force.csv', stringsAsFactors = FALSE, header= TRUE)

BloomUOF2018First<-read.csv(file = 'dirty data/Bloomington/2018 First Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2018Second<-read.csv(file = 'dirty data/Bloomington/2018 Second Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2018Third<-read.csv(file = 'dirty data/Bloomington/2018 Third Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2018Fourth<-read.csv(file = 'dirty data/Bloomington/2018-fourth-quarter-use-of-force (1).csv', stringsAsFactors = FALSE, header= TRUE)

BloomUOF2019First<-read.csv(file = 'dirty data/Bloomington/2019 First Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2019Second<-read.csv(file = 'dirty data/Bloomington/2019 Second Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2019Third<-read.csv(file = 'dirty data/Bloomington/2019-third-quarter-use-of-force.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2019Fourth<-read.csv(file = 'dirty data/Bloomington/2019-fourth-quarter-use-of-force.csv', stringsAsFactors = FALSE, header= TRUE)

BloomUOF2020First<-read.csv(file = 'dirty data/Bloomington/2020 First Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2020Second<-read.csv(file = 'dirty data/Bloomington/2020 Second Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2020Third<-read.csv(file = 'dirty data/Bloomington/2020 Third Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)
BloomUOF2020Fourth<-read.csv(file = 'dirty data/Bloomington/2020 Fourth Quarter UOF.csv', stringsAsFactors = FALSE, header= TRUE)

#some have arbitrary columns that are just row numbers, so we're deleting those rows
FixCol_BloomUOF2016First<- BloomUOF2016First[-c (1) ]
FixCol_BloomUOF2016Second<- BloomUOF2016Second[-c (1) ]
FixCol_BloomUOF2016Third<- BloomUOF2016Third[-c (1) ]
FixCol_BloomUOF2016Fourth<- BloomUOF2016Fourth[-c (1) ]

FixCol_BloomUOF2017First<- BloomUOF2017First[-c (1) ]

FixCol_BloomUOF2018First<- BloomUOF2018First[-c (1) ]
FixCol_BloomUOF2018Second<- BloomUOF2018Second[-c (1) ]
FixCol_BloomUOF2018Third<-BloomUOF2018Third[-c (1) ]

FixCol_BloomUOF2019First<- BloomUOF2019First[-c (1) ]
FixCol_BloomUOF2019Second<- BloomUOF2019Second[-c (1) ]

FixCol_BloomUOF2020First<- BloomUOF2020First[-c (1) ]
FixCol_BloomUOF2020Second<- BloomUOF2020Second[-c (1) ]
FixCol_BloomUOF2020Third<-BloomUOF2020Third[-c (1) ]
FixCol_BloomUOF2020Fourth<- BloomUOF2020Fourth[-c (1) ]

#Bloom2018 First Doesn't Have a couple race columns so I'm going to add them as NA's they all merge well
FixCol_BloomUOF2018First['Suspect.Race.Indian.Alaskan.Native..Hispanic'] <- NA
FixCol_BloomUOF2018First['Suspect.Race.Asian.Pacific.Islander..Non.Hispanic'] <- NA
FixCol_BloomUOF2018First['Suspect.Race.Asian.Pacific.Islander..Hispanic'] <- NA

#fixing column names to all be the same to bind each year together

colnames(BloomUOF2017Second)[1] <- "Event"
colnames(BloomUOF2017Third)[1] <- "Event"
colnames(BloomUOF2017Fourth)[1] <- "Event"

colnames(BloomUOF2018Fourth)[1] <- "Event"

colnames(BloomUOF2019Third)[1] <- "Event"
colnames(BloomUOF2019Fourth)[1] <- "Event"

#fixing 2020's because 2020 fourth has wrong names and extra columns
colnames(FixCol_BloomUOF2020Fourth)[1] <- "Event"
colnames(FixCol_BloomUOF2020Fourth)[10] <- "Drew.Weapon"

FixCol_BloomUOF2020First['Launch.Projectile'] <- NA
FixCol_BloomUOF2020Second['Launch.Projectile'] <- NA
FixCol_BloomUOF2020Third['Launch.Projectile'] <- NA
FixCol_BloomUOF2020Fourth['Less.Lethal'] <- NA

FixCol_BloomUOF2020First <- FixCol_BloomUOF2020First[, c(1:7, 31, 8:30)]
FixCol_BloomUOF2020Second <- FixCol_BloomUOF2020Second[, c(1:7, 31, 8:30)]
FixCol_BloomUOF2020Third <- FixCol_BloomUOF2020Third[, c(1:7, 31, 8:30)]
FixCol_BloomUOF2020Fourth <- FixCol_BloomUOF2020Fourth[, c(1:8, 31, 9:30)]



#binding the years together so there are less individual datasets to deal with
UOF2016<- smartbind(FixCol_BloomUOF2016First,FixCol_BloomUOF2016Second,FixCol_BloomUOF2016Third,FixCol_BloomUOF2016Fourth)

UOF2017<- smartbind(FixCol_BloomUOF2017First,BloomUOF2017Second,BloomUOF2017Third,BloomUOF2017Fourth)

UOF2018<- smartbind(FixCol_BloomUOF2018First,FixCol_BloomUOF2018Second,FixCol_BloomUOF2018Third,BloomUOF2018Fourth)

UOF2019<- smartbind(FixCol_BloomUOF2019First,FixCol_BloomUOF2019Second,BloomUOF2019Third,BloomUOF2019Fourth)

UOF2020<- smartbind(FixCol_BloomUOF2020First,FixCol_BloomUOF2020Second,FixCol_BloomUOF2020Third,FixCol_BloomUOF2020Fourth)


#fixing columns in each year to all match to bind them all into one UOF data set
colnames(UOF2020)[10] <- "Draw.Less.Lethal"
colnames(UOF2020)[11] <- "Draw.Weapon"
colnames(UOF2020)[24] <- "Suspect.Race.Caucasian..Non.Hispanic"
colnames(UOF2020)[25] <- "Suspect.Race..Caucasian..Hispanic"
colnames(UOF2020)[26] <- "Suspect.Race.African.American..Non.Hispanic"
colnames(UOF2020)[27] <- "Suspect.Race.African.American..Hispanic"




UOF2016['Draw.Less.Lethal'] <- NA
UOF2017['Draw.Less.Lethal'] <- NA
UOF2018['Draw.Less.Lethal'] <- NA
UOF2019['Draw.Less.Lethal'] <- NA

UOF2016['Launch.Projectile'] <- NA
UOF2017['Launch.Projectile'] <- NA
UOF2018['Launch.Projectile'] <- NA
UOF2019['Launch.Projectile'] <- NA

#changing column order so all datasets are the same
UOF2016 <- UOF2016[, c(1:7, 31, 8, 30, 9:29)]
UOF2017 <- UOF2017[, c(1:7, 31, 8, 30, 9:29)]
UOF2018 <- UOF2018[, c(1:7, 31, 8, 30, 9:29)]
UOF2019 <- UOF2019[, c(1:7, 31, 8, 30, 9:29)]


#binding them all into one dataset
All_UOF<- smartbind(UOF2016,UOF2017,UOF2018,UOF2019,UOF2020)

#changing the individual force columns to their binnings numbers rather than Yes and No
All_UOF$Physical.Contact <- gsub('YES|Yes', "1", All_UOF$Physical.Contact)
All_UOF$Physical.Contact <- gsub('NO|No', NA, All_UOF$Physical.Contact)

All_UOF$Launch.Projectile <- gsub('YES|Yes', "1", All_UOF$Launch.Projectile)
All_UOF$Launch.Projectile <- gsub('NO|No', NA, All_UOF$Launch.Projectile)

All_UOF$Less.Lethal <- gsub('YES|Yes', "2", All_UOF$Less.Lethal)
All_UOF$Less.Lethal <- gsub('NO|No', NA, All_UOF$Less.Lethal)

All_UOF$Draw.Less.Lethal <- gsub('YES|Yes', "1", All_UOF$Draw.Less.Lethal)
All_UOF$Draw.Less.Lethal <- gsub('NO|No', NA, All_UOF$Draw.Less.Lethal)

All_UOF$Draw.Weapon <- gsub('YES|Yes', "1", All_UOF$Draw.Weapon)
All_UOF$Draw.Weapon <- gsub('NO|No', NA, All_UOF$Draw.Weapon)

All_UOF$Weapon.Fired <- gsub('YES|Yes', "3", All_UOF$Weapon.Fired)
All_UOF$Weapon.Fired <- gsub('NO|No', NA, All_UOF$Weapon.Fired)

All_UOF$Vehicle.Pursuit <- gsub('YES|Yes', "1", All_UOF$Vehicle.Pursuit)
All_UOF$Vehicle.Pursuit <- gsub('NO|No', NA, All_UOF$Vehicle.Pursuit)

All_UOF$Foot.Pursuit <- gsub('YES|Yes', "1", All_UOF$Foot.Pursuit)
All_UOF$Foot.Pursuit <- gsub('NO|No', NA, All_UOF$Foot.Pursuit)


#uniting these into one Force Level Column
All_UOF_ForceLevel <- All_UOF
All_UOF_ForceLevel <- unite(All_UOF_ForceLevel, "Force.Level", 7:14, sep = ", ", remove = TRUE, na.rm = TRUE)


All_UOF_ForceLevel <- All_UOF_ForceLevel %>%
  mutate(Force.Level = case_when(
    str_detect(Force.Level, "3") ~ "3",
    str_detect(Force.Level, "2") ~ "2",
    str_detect(Force.Level, "1") ~ "1",
    TRUE ~ Force.Level
  ))

#Fixing empty columns to NAs
All_UOF_ForceLevel$Force.Level[All_UOF_ForceLevel$Force.Level==", , , , , , "]<- NA
All_UOF_ForceLevel$Force.Level[All_UOF_ForceLevel$Force.Level==""]<- NA



#Fixing Gender columns
All_UOF_ForceLevel$Suspect.Gender.Female[All_UOF_ForceLevel$Suspect.Gender.Female=="1"]<-"Female"
All_UOF_ForceLevel$Suspect.Gender.Male[All_UOF_ForceLevel$Suspect.Gender.Male=="1"]<-"Male"

All_UOF_ForceLevel <- unite(All_UOF_ForceLevel, "Suspect.Gender", 14:15, sep = ", ", remove = TRUE, na.rm = T)
All_UOF_ForceLevel$Suspect.Gender[All_UOF_ForceLevel$Suspect.Gender==""]<-NA

#fixing Race columns
All_UOF_ForceLevel$Suspect.Race.Caucasian..Non.Hispanic[All_UOF_ForceLevel$Suspect.Race.Caucasian..Non.Hispanic=="1"]<- "White"
All_UOF_ForceLevel$Suspect.Race..Caucasian..Hispanic [All_UOF_ForceLevel$Suspect.Race..Caucasian..Hispanic=="1"]<- "Hispanic"
All_UOF_ForceLevel$Suspect.Race.African.American..Non.Hispanic[All_UOF_ForceLevel$Suspect.Race.African.American..Non.Hispanic=="1"]<- "Black"
All_UOF_ForceLevel$Suspect.Race.African.American..Hispanic[All_UOF_ForceLevel$Suspect.Race.African.American..Hispanic=="1"]<- "Black & Hispanic"
All_UOF_ForceLevel$Suspect.Race.Indian.Alaskan.Native..Non.Hispanic[All_UOF_ForceLevel$Suspect.Race.Indian.Alaskan.Native..Non.Hispanic=="1"]<- "Native American"
All_UOF_ForceLevel$Suspect.Race.Indian.Alaskan.Native..Hispanic[All_UOF_ForceLevel$Suspect.Race.Indian.Alaskan.Native..Hispanic=="1"]<- "Native American & Hispanic"
All_UOF_ForceLevel$Suspect.Race.Asian.Pacific.Islander..Non.Hispanic[All_UOF_ForceLevel$Suspect.Race.Asian.Pacific.Islander..Non.Hispanic=="1"]<- "Asian/Pacific Islander"
All_UOF_ForceLevel$Suspect.Race.Asian.Pacific.Islander..Hispanic[All_UOF_ForceLevel$Suspect.Race.Asian.Pacific.Islander..Hispanic=="1"]<- "Asian/Pacific Islander & Hispanic"

All_UOF_FixRace <- All_UOF_ForceLevel
All_UOF_FixRace <- unite(All_UOF_FixRace, "Suspect.Race", 16:23, sep = ", ", remove = T, na.rm = T)

#writing a new CSV with cleaned data
write.csv(All_UOF_FixRace,"clean data/Bloomington/UOF.csv",row.names = FALSE)


