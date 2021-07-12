#loading libraries
library(here)
library(dplyr)
library(tidyr)

#loading datasets
UOF <- read.csv(file=here('dirty data/New Orleans/NOPD_Use_of_Force_Incidents.csv'), stringsAsFactors = FALSE)

#making a table with relevant data
AllMetadata_UOF<- cbind.data.frame(UOF$PIB.File.Number,UOF$Date.Occurred,UOF$Disposition,UOF$Service.Type,UOF$Use.of.Force.Type,UOF$Use.of.Force.Level,UOF$Officer.Race.Ethnicity,UOF$Officer.Gender,UOF$Officer.Age,UOF$Officer.Years.of.Service,UOF$Subject.Gender,UOF$Subject.Ethnicity,UOF$Subject.Age,UOF$Subject.Injured,UOF$Use.of.Force.Reason,UOF$Officer.Injured, stringsAsFactors=FALSE)
colnames(AllMetadata_UOF)<-(c("PIB File Number","Date","Disposition","Service Type","Force Type","Force Level","Officer Race","Officer Gender","Officer Age","Officers Yrs of Service","Subject Gender","Subject Race","Subject Age","Subject Injured","UOF Reason","Officer Injured"))


#separating rows to be one offender per row
UOF_Officers <- AllMetadata_UOF
UOF_Offiers<- UOF_Officers %>% separate_rows("Officer Race", "Officer Gender", "Officer Age","Officers Yrs of Service",  sep= "|")
