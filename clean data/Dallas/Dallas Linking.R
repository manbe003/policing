#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
PackageDependency()

#reading in a large dataframe that contains both officer badge number and name 
DallasLinker <- read.csv(unz("Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.zip", "Dallas_Police_Public_Data_-_RMS_Incidents-With_GeoLocation.csv"))
#taking just officer badge number and name from this dataset
DallasLinker<-cbind.data.frame(DallasLinker[, c("RO1Name", "RO1Badge")])
#deleting duplicate rows 
DallasLinker<-unique(DallasLinker)
#changing column names 
colnames(DallasLinker) <- c("officer_name", "officer_badge_number")
#putting the officer name column into the format of "Firstname Lastname" to match the other datasets
DallasLinker <-separate(DallasLinker, officer_name, c('last_name', 'first_name', 'middle_name'), sep=",")
DallasLinker[ ,c("middle_name")] <- list(NULL)
DallasLinker$first_name <- tolower(DallasLinker$first_name)
DallasLinker$last_name <- tolower(DallasLinker$last_name)
DallasLinker$last_name <- str_to_title(DallasLinker$last_name)
DallasLinker$first_name <- str_to_title(DallasLinker$first_name)
DallasLinker$officer_name <- paste(DallasLinker$first_name, DallasLinker$last_name)
DallasLinker[ ,c("first_name", "last_name")] <- list(NULL)
#Exporting this dataset to dirty data
write.csv(DallasLinker,"~/Desktop/policing/dirty data/Dallas/DallasLink.csv",row.names = FALSE)