

library(utils)
DallasLinker <- read.csv(unz("Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.zip", "Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.csv"))


DallasLinker<-cbind.data.frame(DallasLinker[, c("RO1Name", "RO1Badge")])
DallasLinker<-unique(DallasLinker)
colnames(DallasLinker) <- c("officer_name", "officer_badge_number")

DallasLinker <- DallasLinkerOriginal

DallasLinker <-separate(DallasLinker, RO1Name, c('last_name', 'first_name', 'middle_name'), sep=",")

DallasLinker[ ,c("middle_name")] <- list(NULL)

DallasLinker$first_name <- tolower(DallasLinker$first_name)
DallasLinker$last_name <- tolower(DallasLinker$last_name)
DallasLinker$last_name <- str_to_title(DallasLinker$last_name)
DallasLinker$first_name <- str_to_title(DallasLinker$first_name)

DallasLinker$officer_name <- paste(DallasLinker$first_name, DallasLinker$last_name)

DallasLinker[ ,c("first_name", "last_name")] <- list(NULL)
View(DallasLinker)
setwd("~/Desktop/policing/dirty data/Dallas")

write.csv(DallasLinker,"~/Desktop/policing/dirty data/Dallas/DallasLink.csv",row.names = FALSE)
