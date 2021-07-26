
Dallas_Shootings <- read.csv("Dallas_shootings.csv", stringsAsFactors = FALSE)
View(Dallas_Shootings)
library(utils)
setwd("~/Desktop/policing/dirty data/Dallas")

#None of this works yet - start at the next #
Dallas_R2R_2018 <- read.csv("Police_Response_to_Resistance_-_2018.csv", stringsAsFactors = FALSE)
Dallas_R2R_2018 <- cbind.data.frame(Dallas_R2R_2018[, c("FILENUM", "CURRENT_BAxx")])
colnames(Dallas_R2R_2018) <- c("FILENUM", "officer_badge_number")

DallasLinker2018 <- read.csv("officer names 2018.csv", stringsAsFactors = FALSE)
DallasLinker2018 <- unique(DallasLinker2018)
colnames(DallasLinker2018) <- c("FILENUM", "last_name", "first_name", "age")
DallasLinker2018$officer_name <- paste(DallasLinker2018$first_name, DallasLinker2018$last_name)
DallasLinker2018$officer_name <- sub("  ", " ", DallasLinker2018$officer_name)
DallasLinker2018[ ,c("age", "first_name", "last_name")] <- list(NULL)

DallasLinker2018 <- merge(Dallas_R2R_2018, DallasLinker2018, by = "FILENUM", all.y = TRUE)

#Here is where the working code starts.
DallasLinker <- read.csv(unz("Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.zip", "Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.csv"))

DallasLinker<-cbind.data.frame(DallasLinker[, c("RO1Name", "RO1Badge")])
DallasLinker<-unique(DallasLinker)
colnames(DallasLinker) <- c("officer_name", "officer_badge_number")

DallasLinkerOriginal <- DallasLinker

DallasLinker <-separate(DallasLinker, officer_name, c('last_name', 'first_name', 'middle_name'), sep=",")

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

View(DallasLinker)
