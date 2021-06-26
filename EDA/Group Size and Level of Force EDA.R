Libraries
library(here)
library(tidyverse)
library(dplyr)


#loading in datasets
louisville_OIS <- read.csv(file = here("clean data/Louisville/LouisvilleShootings.csv"), stringsAsFactors = FALSE)
Seattle_OIS<- read.csv(file = here("clean data/seattle/shootings_Seattle.csv"), stringsAsFactors = FALSE)
Seattle_UOF<- read.csv(file = here("clean data/seattle/UseOfForce_Seattle.csv"), stringsAsFactors = FALSE)
Indianapolis_UOF<- read.csv(file = here("clean data/Indianapolis/UOF.csv"), stringsAsFactors = FALSE)
Orlando_UOF <- read.csv(file = here("clean data/Orlando/UOF (cleaned).csv"), stringsAsFactors = FALSE)
Orlando_OIS <- read.csv(file = here("clean data/Orlando/shooting (cleaned).csv"), stringsAsFactors = FALSE)


#Zoes function for adding a group size column
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

#Adding the groupsize column and making a new dataset with duplicate cases removed
louisville_OIS<-OfficerGroupSize(louisville_OIS, "incident_number", louisville_OIS$incident_number)
louisvilleDistinct_OIS<- distinct(louisville_OIS,incident_number, .keep_all= TRUE)

Seattle_OIS<- OfficerGroupSize(Seattle_OIS, "GO", Seattle_OIS$GO)
SeattleDistinct_OIS<- distinct(Seattle_OIS,GO, .keep_all= TRUE)

Seattle_UOF<- OfficerGroupSize(Seattle_UOF, "Incident_Num", Seattle_UOF$Incident_Num)
SeattleDistinct_UOF<- distinct(Seattle_UOF,Incident_Num, .keep_all= TRUE)

Indianapolis_UOF<- OfficerGroupSize(Indianapolis_UOF, "id", Indianapolis_UOF$id)
IndianapolisDistinct_UOF<- distinct(Indianapolis_UOF,id, .keep_all= TRUE)

Orlando_OIS <- Orlando_OIS %>% separate_rows(Officer.Name, sep = ";")
Orlando_OIS<-OfficerGroupSize(Orlando_OIS, "Incident.Number", Orlando_OIS$Incident.Number)
OrlandoDistinct_OIS<- distinct(Orlando_OIS, Incident.Number, .keep_all= TRUE)

##Graphs of Group sizes for OIS
ggplot(louisvilleDistinct_OIS, aes(officer_group_size))+
  geom_bar()

ggplot(SeattleDistinct_OIS, aes(officer_group_size))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,15,by=1))



##graphs of group sizes for UOF
ggplot(SeattleDistinct_UOF, aes(officer_group_size))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,10,by=1))

print(table(SeattleDistinct_UOF$officer_group_size))


ggplot(OrlandoDistinct_OIS, aes(officer_group_size))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,10,by=1))

#UOF graphs and tables
ggplot(Orlando_UOF, aes(Officers.Involved))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,12,by=1))

print(table(Orlando_UOF$Officers.Involved))


ggplot(IndianapolisDistinct_UOF, aes(officer_group_size))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,50,by=5))

print(table(IndianapolisDistinct_UOF$officer_group_size))



########################################################################################################################
#Cleaning up the UOF


#Seattle binning UOF level according to paper outline
SeattleDistinct_UOF$Incident_Type<- gsub('Level 1 - Use of Force',"1" ,SeattleDistinct_UOF$Incident_Type)
SeattleDistinct_UOF$Incident_Type<- gsub('Level 2 - Use of Force',"2" ,SeattleDistinct_UOF$Incident_Type)
SeattleDistinct_UOF$Incident_Type<- gsub('Level 3 - Use of Force|Level 3 - OIS',"3" ,SeattleDistinct_UOF$Incident_Type)

ggplot(SeattleDistinct_UOF, aes(Incident_Type))+
  geom_bar()

#Seattle binning officers according to paper outline
SeattleDistinct_UOF_Off <- SeattleDistinct_UOF
SeattleDistinct_UOF_Off$officer_group_size<- gsub('1',"0" ,SeattleDistinct_UOF_Off$officer_group_size)
SeattleDistinct_UOF_Off$officer_group_size<- gsub('2',"1" ,SeattleDistinct_UOF_Off$officer_group_size)
SeattleDistinct_UOF_Off$officer_group_size<- gsub('3|4|5|6|9',"2" ,SeattleDistinct_UOF_Off$officer_group_size)

print(table(SeattleDistinct_UOF_Off$officer_group_size))

ggplot(SeattleDistinct_UOF_Off, aes(officer_group_size))+
  geom_bar()

ggplot(SeattleDistinct_UOF_Off,
       aes(x = officer_group_size,
           fill = Incident_Type))+
  geom_bar(position = "dodge")


#Indianapolis Key:binning UOF level according to paper outline
IndianapolisDistinct_UOF$officerForceType<- gsub('Physical-Kick|Physical-Hands, Fist, Feet|Physical-Weight Leverage|Physical-Take Down|Physical-Palm Strike|Physical-Elbow Strike|Physical-Handcuffing|Physical-Leg Sweep|Physical-Knee Strike|Physical-Push|Physical-Other|Physical-Joint/Pressure|Physical-Fist Strike','1',IndianapolisDistinct_UOF$officerForceType)
IndianapolisDistinct_UOF$officerForceType<- gsub('Less Lethal-Taser|Less Lethal-Personal CS/OC spray|Less Lethal-Baton|Less Lethal-Burning CS|Less Lethal-Flash Bang|Less Lethal-Pepperball|Less Lethal-Bps Gas|Less Lethal-CS Grenade|Less Lethal-Other|Less Lethal-CS/OC|Less Lethal-Clearout OC|Less Lethal-Bean Bag|Less Lethal-CS Fogger|Canine Bite','2',IndianapolisDistinct_UOF$officerForceType)
IndianapolisDistinct_UOF$officerForceType<- gsub('Lethal-Handgun|Lethal-Vehicle','3',IndianapolisDistinct_UOF$officerForceType)
IndianapolisDistinct_UOF$officerForceType<- gsub('N/A',NA,IndianapolisDistinct_UOF$officerForceType)

ggplot(IndianapolisDistinct_UOF, aes(officerForceType))+
  geom_bar()

#Indianapolis Binning officers according to paper outline
IndianapolisDistinct_UOF_Off <- IndianapolisDistinct_UOF
IndianapolisDistinct_UOF_Off$officer_group_size<- gsub("^1$","0" ,IndianapolisDistinct_UOF_Off$officer_group_size)
IndianapolisDistinct_UOF_Off$officer_group_size<- gsub("^2$","1" ,IndianapolisDistinct_UOF_Off$officer_group_size)
IndianapolisDistinct_UOF_Off$officer_group_size<- gsub("3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|24|26|27|28|31|46","2" ,IndianapolisDistinct_UOF_Off$officer_group_size)

print(table(IndianapolisDistinct_UOF_Off$officer_group_size))

ggplot(IndianapolisDistinct_UOF_Off, aes(officer_group_size))+
  geom_bar()

ggplot(IndianapolisDistinct_UOF_Off,
       aes(x = officer_group_size,
           fill = officerForceType))+
  geom_bar(position = "dodge")



#Orlando Key: 3= physical, 4= less than lethal force
Orlando_UOF$Electronic.Device.Used[Orlando_UOF$Electronic.Device.Used=="Yes"]<-("4")
Orlando_UOF$Chemical.Agent.Used[Orlando_UOF$Chemical.Agent.Used=="Yes"]<-("4")
Orlando_UOF$Tackle.Take.Down[Orlando_UOF$Tackle.Take.Down=="Yes"]<-("3")
Orlando_UOF$Impact.Weapons.Used[Orlando_UOF$Impact.Weapons.Used=="Yes"]<-("4")
Orlando_UOF$Physical.Strikes.Made[Orlando_UOF$Physical.Strikes.Made=="Yes"]<-("3")
Orlando_UOF$Deflation.Devices.Used[Orlando_UOF$Deflation.Devices.Used=="Yes"]<-("4")
Orlando_UOF$K9.Unit.Involved[Orlando_UOF$K9.Unit.Involved=="Yes"]<-("4")
Orlando_UOF[Orlando_UOF=="No"]<-NA

Orlando_UOF<- Orlando_UOF %>% unite("UOF.Level", Electronic.Device.Used:K9.Unit.Involved, sep = ";", na.rm = TRUE, remove = TRUE)

Orlando_UOF$UOF.Level<- gsub('4|3;4|4;3|4;4;3|4;3;4|4;4;4|4;4|4;4;3;3|4;3;3|4;4;3;4|4;4;3;4;3|4;4;4;3|3;3;4|3;4;4|4;3;4;3|4;3;4;3;4|3;4;3|4;4;3;4;3;4|3;4;3;4|4;4;3;3;4', '2', Orlando_UOF$UOF.Level)
Orlando_UOF$UOF.Level<- gsub('3|3;3','1',Orlando_UOF$UOF.Level)
Orlando_UOF[Orlando_UOF==""]<-NA


ggplot(Orlando_UOF, aes(UOF.Level))+
  geom_bar()


#Orlando Binning according to paper notes
Orlando_UOF_Off <- Orlando_UOF
Orlando_UOF_Off$Officers.Involved<- gsub("^1$","0" ,Orlando_UOF_Off$Officers.Involved)
Orlando_UOF_Off$Officers.Involved<- gsub("^2$","1" ,Orlando_UOF_Off$Officers.Involved)
Orlando_UOF_Off$Officers.Involved<- gsub("3|4|5|6|7|8|9|10|11|12","2" ,Orlando_UOF_Off$Officers.Involved)

print(table(Orlando_UOF_Off$Officers.Involved))

ggplot(Orlando_UOF_Off, aes(Officers.Involved))+
  geom_bar()

ggplot(Orlando_UOF_Off,
       aes(x = Officers.Involved,
           fill = UOF.Level))+
  geom_bar(position = "dodge")





#Graphs compairing UOF level and original Groupsize
ggplot(SeattleDistinct_UOF,
       aes(x = officer_group_size,
           fill = Incident_Type))+
  geom_bar(position = "dodge")+
  scale_x_continuous(breaks=seq(0,10,by=1))


ggplot(IndianapolisDistinct_UOF,
       aes(x = officer_group_size,
           fill = officerForceType))+
  geom_bar(position = "dodge")+
  scale_x_continuous(breaks=seq(0,50,by=5))



ggplot(Orlando_UOF,
       aes(x = Officers.Involved,
           fill = UOF.Level))+
  geom_bar(position = "dodge")+
  scale_x_continuous(breaks=seq(0,12,by=1))

