#first I want to call libraries 
library (dplyr)
library (tidyr)

#set working directory
setwd("~/Desktop/policing/dirty data/Louisville")

#I want to call in my datasets (citations, use of force)

LouisvilleStops<-read.csv(file = 'Louisville stops.csv', stringsAsFactors = FALSE)

setwd("~/Desktop/policing/dirty data/Louisville")
LouisvilleShootings<-read.csv(file = 'all_louisville_OIS.csv', stringsAsFactors = FALSE)

View(LouisvilleShootings)
#making a table with all relevant metadata for stops
LouisvilleStops<-cbind.data.frame(stops$ID,stops$TYPE_OF_STOP,stops$CITATION_CONTROL_NUMBER,stops$ACTIVITY.RESULTS,stops$OFFICER_GENDER,stops$OFFICER_RACE,stops$OFFICER_AGE_RANGE,stops$ACTIVITY_DATE,stops$ACTIVITY_TIME,stops$ACTIVITY_LOCATION,stops$ACTIVITY_DIVISION,stops$ACTIVITY_BEAT,stops$DRIVER_GENDER,stops$DRIVER_RACE,stops$DRIVER_AGE_RANGE,stops$NUMBER.OF.PASSENGERS,stops$WAS_VEHCILE_SEARCHED,stops$REASON_FOR_SEARCH, stringsAsFactors=FALSE)
colnames(LouisvilleStops)<-(c("ID","type_of_stop","citation_control_number","activity.results","officer_gender","officer_race","officer_age_range","activity_date","activity_time","activity_location", "activity_division","activity_beat","driver_gender","driver_race","driver_age_range","number.of.passengers","was_vehcile_searched","reason_for_search"))

#make all null values = NA for stops
AllMetadata_stops_NA<-AllMetadata_stops
AllMetadata_stops_NA[AllMetadata_stops=="0"]<-NA
AllMetadata_stops_NA[AllMetadata_stops=="-"]<-NA

#Change race to be consisted with citations
AllMetadata_stops_FixRace<-AllMetadata_stops_NA
AllMetadata_stops_FixRace[AllMetadata_stops=="INDIAN/INDIA/BURMESE"]<-"INDIAN"
AllMetadata_stops_FixRace[AllMetadata_stops=="ASIAN/PACIFIC ISLANDER"]<-"ASIAN"

#write the file to a new location in clean data folder!
write.csv(AllMetadata_stops,"/Users/aryaajwani/Desktop/Research/policing/clean data/Louisville/stops_Louisville_clean",row.names = FALSE)
