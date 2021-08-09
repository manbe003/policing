#loading libraries
library(here)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(stringi)

#loading datasets
UOF <- read.csv(file=here('dirty data/New Orleans/NOPD_Use_of_Force_Incidents.csv'), stringsAsFactors = FALSE)

#making a table with relevant data
AllMetadata_UOF<- cbind.data.frame(UOF$PIB.File.Number,UOF$Date.Occurred,UOF$Disposition,UOF$Service.Type,UOF$Use.of.Force.Type,UOF$Use.of.Force.Level,UOF$Officer.Race.Ethnicity,UOF$Officer.Gender,UOF$Officer.Age,UOF$Officer.Years.of.Service,UOF$Subject.Gender,UOF$Subject.Ethnicity,UOF$Subject.Age,UOF$Subject.Injured,UOF$Use.of.Force.Reason,UOF$Officer.Injured, stringsAsFactors=FALSE)
colnames(AllMetadata_UOF)<-(c("PIB File Number","Date","Disposition","Service Type","Force Type","Force Level","Officer Race","Officer Gender","Officer Age","Officers Yrs of Service","Subject Gender","Subject Race","Subject Age","Subject Injured","UOF Reason","Officer Injured"))

#making blanks NA
AllMetadata_UOF_NA<- AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF_NA==""]<-NA

#binning force type according to paper notes
UOF_All_FixLevels <- UOF_All_FixLevels %>%
    mutate(`PD Force Type` = case_when(
    str_detect(`PD Force Type`, "Firearm") ~ "3",
    str_detect(`PD Force Type`, "Escort Tech") ~ "2",
    str_detect(`PD Force Type`, "CEW") ~ "2",
    str_detect(`PD Force Type`, "Canine") ~ "2",
    str_detect(`PD Force Type`, "Hands") ~ "3",
    str_detect(`PD Force Type`, "Take Down") ~ "1",
    TRUE ~ `PD Force Type`
    ))



separate_rows(Force Level, sep = ";") %>%
    separate(location, c("Officer Race", "value")) %>%
    filter(location_type %in% c("country", "city")) %>%
    spread(location_type, value)


UOF_Officers<- AllMetadata_UOF_NA
UOF_Officers$number.of.officers <- str_count(UOF_Officers$`Officer Race`, "|")
UOF_Officers$number.of.officers <- string.counter(strings=UOF_Officers$`Officer Race`, pattern="a")

UOF_Officers$number.of.officers <- str_count(UOF_Officers$`Officer Race`, fixed = "|")
UOF_Officers<-cSplit(UOF_Officers, "Officer Race","|")













#separating rows to be one officer per row
UOF_Officers<- AllMetadata_UOF_NA
UOF_Officers<- UOF_Officers %>% separate_rows("Force Level","Officer Race", "Officer Gender", "Officer Age","Officers Yrs of Service", sep= "|")
UOF_Officers<- UOF_Officers[-c(1391, 1709, 1710, 2214, 2250, 1393, 1332, 241, 243, 734, 1622, 1736, 2068, 2338, 2399, 98, 226, 683, 1105, 1170, 1402, 1584, 1644, 1727, 1818, 1896, 1945, 1985, 2006, 2190, 2451, 2484, 1365, 2550, 1393, 1332, 241, 243, 734, 1622, 1736, 2068, 2338, 2399, 98, 226), ]
UOF_Officers<- UOF_Officers %>% separate_rows("Force Level","Officer Race", "Officer Gender", "Officer Age","Officers Yrs of Service", sep= "|")

#trying to see what the length of each is to see if we can find the incompatible ones
UOF_Officers_Test1<- UOF_Officers
UOF_Officers_Test1$test = mutate(UOF_Officers_Test1,nchar(UOF_Officers_Test1$`Officer Age`))

UOF_Officers_Test2<- UOF_Officers
UOF_Officers_Test2$test = mutate(UOF_Officers_Test2,nchar(UOF_Officers_Test2$`Officer Gender`))

UOF_Officers_Test3<- UOF_Officers
UOF_Officers_Test3$test = mutate(UOF_Officers_Test3,nchar(UOF_Officers_Test3$`Force Level`))

UOF_Officers_Test4<- UOF_Officers
UOF_Officers_Test4$test = mutate(UOF_Officers_Test4,nchar(UOF_Officers_Test4$`Officer Age`))

UOF_Officers_Test5<- UOF_Officers
UOF_Officers_Test5$test = mutate(UOF_Officers_Test5,nchar(UOF_Officers_Test5$`Officers Yrs of Service`))


UOF_Officers <- AllMetadata_UOF
UOF_Officers<- UOF_Officers %>% separate_rows(`Force Level`,`Officer Race`, `Officer Age`, `Officer Gender`, `Officer Age`,`Officers Yrs of Service`, sep= "|")
test = mutate(UOF_Officers, length = nchar(UOF_Officers$`Force Level`))
sort(test$length, decreasing = TRUE)







