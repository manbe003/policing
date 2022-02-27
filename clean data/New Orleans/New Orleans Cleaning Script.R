#load dependencies and set working directory
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()
setwd(here())

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
UOF_Officers <- AllMetadata_UOF_NA
UOF_Officers['Number of Officers'] <- NA
UOF_Officers['Binning Number of Officers'] <- NA
UOF_Officers$`Number of Officers` <- str_count(UOF_Officers$`Officer Gender`, coll("|"))+1
UOF_Officers$`Binning Number of Officers`[UOF_Officers$`Number of Officers`=="1"]<- "1"
UOF_Officers$`Binning Number of Officers`[UOF_Officers$`Number of Officers`=="2"]<- "2"
UOF_Officers$`Binning Number of Officers`[UOF_Officers$`Number of Officers` > "2"]<- "3+"

  
#Binning Force Type
UOF_Officers$Force.Type.Binning <- UOF_Officers$`Force Type`
UOF_Officers <- UOF_Officers %>%
  mutate(Force.Type.Binning = case_when(
    str_detect(Force.Type.Binning, "Discharged") ~ "3",
    str_detect(Force.Type.Binning, "Vehicle as Weapon") ~ "3",
    str_detect(Force.Type.Binning, "Escort Tech") ~ "2",
    str_detect(Force.Type.Binning, "CEW") ~ "2",
    str_detect(Force.Type.Binning, "Canine") ~ "2",
    str_detect(Force.Type.Binning, "Baton") ~ "2",
    str_detect(Force.Type.Binning, "NonTrad Impact Weapon") ~ "2",
    str_detect(Force.Type.Binning, "Pointed") ~ "1",
    str_detect(Force.Type.Binning, "Exhibited") ~ "1",
    str_detect(Force.Type.Binning, "Canine (No Bite)") ~ "1",
    str_detect(Force.Type.Binning, "Hands") ~ "1",
    str_detect(Force.Type.Binning, "Take Down") ~ "1",
    str_detect(Force.Type.Binning, "Takedown") ~ "1",
    str_detect(Force.Type.Binning, "Head Strike") ~ "1",
    str_detect(Force.Type.Binning, "Force") ~ "1",
    str_detect(Force.Type.Binning, "Handcuffed Subject") ~ "1",
    TRUE ~ Force.Type.Binning
  ))


write.csv(UOF_Officers,"clean data/New Orleans/New Orleans UOF.csv",row.names = FALSE)