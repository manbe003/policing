#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
PackageDependency()

#loading datasets
UOF <- read.csv(file='dirty data/New Orleans/NOPD_Use_of_Force_Incidents.csv', stringsAsFactors = FALSE)

#making a table with relevant data
AllMetadata_UOF<- cbind.data.frame(UOF$PIB.File.Number,UOF$Date.Occurred,UOF$Disposition,UOF$Service.Type,UOF$Use.of.Force.Type,UOF$Use.of.Force.Level,UOF$Officer.Race.Ethnicity,UOF$Officer.Gender,UOF$Officer.Age,UOF$Officer.Years.of.Service,UOF$Subject.Gender,UOF$Subject.Ethnicity,UOF$Subject.Age,UOF$Subject.Injured,UOF$Use.of.Force.Reason,UOF$Officer.Injured, stringsAsFactors=FALSE)
colnames(AllMetadata_UOF)<-(c("PIB File Number","Date","Disposition","Service Type","Force Type","Force Level","Officer Race","Officer Gender","Officer Age","Officers Yrs of Service","Subject Gender","Subject Race","Subject Age","Subject Injured","UOF Reason","Officer Injured"))

#making blanks NA
AllMetadata_UOF_NA<- AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF_NA==""]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF_NA=="Other | Other | Other"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF_NA=="Other"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF_NA=="Other | Other"]<-NA

#making a column counting number of officers in a group based on a column and its separator
OfficerBinning <- function (dataframe, dataframecol, separator){
  dataframe['Number of Officers'] <- NA
  dataframe['Binning Number of Officers'] <- NA
  dataframe$`Number of Officers` <- str_count(dataframecol, coll(separator))+1
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="1"]<- "1"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers`=="2"]<- "2"
  dataframe$`Binning Number of Officers`[dataframe$`Number of Officers` > "2"]<- "3+"
  return(dataframe)
  
}

UOF_Officers <- AllMetadata_UOF_NA
UOF_Officers <- OfficerBinning(UOF_Officers, UOF_Officers$`Officer Gender`, "|")


write.csv(UOF_Officers,"clean data/New Orleans/New Orleans UOF.csv",row.names = FALSE)