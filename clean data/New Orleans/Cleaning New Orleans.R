#loading libraries
library(here)
library(dplyr)
library(tidyr)

#loading datasets
UOF <- read.csv(file=here('dirty data/New Orleans/NOPD_Use_of_Force_Incidents.csv'), stringsAsFactors = FALSE)

#making a table with relevant data
AllMetadata_UOF<- cbind.data.frame(UOF$PIB.File.Number,UOF$Date.Occurred,UOF$Disposition,UOF$Service.Type,UOF$Use.of.Force.Type,UOF$Use.of.Force.Level,UOF$Officer.Race.Ethnicity,UOF$Officer.Gender,UOF$Officer.Age,UOF$Officer.Years.of.Service,UOF$Subject.Gender,UOF$Subject.Ethnicity,UOF$Subject.Age,UOF$Subject.Injured,UOF$Use.of.Force.Reason,UOF$Officer.Injured, stringsAsFactors=FALSE)
colnames(AllMetadata_UOF)<-(c("PIB File Number","Date","Disposition","Service Type","Force Type","Force Level","Officer Race","Officer Gender","Officer Age","Officers Yrs of Service","Subject Gender","Subject Race","Subject Age","Subject Injured","UOF Reason","Officer Injured"))

#making blanks NA
AllMetadata_UOF_NA<- AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF_NA==""]<-NA


#separating rows to be one officer per row
UOF_Officers<- AllMetadata_UOF_NA
UOF_Offiers<- UOF_Officers %>% separate_rows("Force Level","Officer Race", "Officer Gender", "Officer Age","Officers Yrs of Service", sep= "|")

#trying to see what the length of each is to see if we can find the incompatible ones
UOF_Officers_Test<- UOF_Officers
UOF_Officers_Test$test = mutate(UOF_Officers_Test,nchar(UOF_Officers_Test$`Officer Age`))
UOF_Officers_Test$test = mutate(UOF_Officers_Test,nchar(UOF_Officers_Test$`Officer Gender`))
UOF_Officers <- AllMetadata_UOF
UOF_Offiers<- UOF_Officers %>% separate_rows("Officer Race", "Officer Gender", "Officer Age","Officers Yrs of Service",  sep= "|")
test = mutate(UOF_Officers, length = nchar(UOF_Officers$`Force Level`))
sort(test$length, decreasing = TRUE)







