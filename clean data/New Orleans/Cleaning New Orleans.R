#loading libraries
library(here)
library(dplyr)

#loading datasets
UOF <- read.csv(file=here('dirty data/New Orleans/NOPD_Use_of_Force_Incidents.csv'), stringsAsFactors = FALSE)

#making a table with relevant data
AllMetadata_UOF<- cbind.data.frame(UOF$PIB.File.Number,UOF$Date.Occurred,UOF$Originating.Bureau,UOF$Disposition,UOF$Service.Type.UOF)