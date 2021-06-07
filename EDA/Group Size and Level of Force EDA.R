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
#Cleaning up the Types of UOF


#Seattle Key- 1= minor discomfort, 2= some injury/harm, 2= serious injury/lethal
SeattleDistinct_UOF$Incident_Type<- gsub('Level 1 - Use of Force',"1" ,SeattleDistinct_UOF$Incident_Type)
SeattleDistinct_UOF$Incident_Type<- gsub('Level 2 - Use of Force',"2" ,SeattleDistinct_UOF$Incident_Type)
SeattleDistinct_UOF$Incident_Type<- gsub('Level 3 - Use of Force|Level 3 - OIS',"3" ,SeattleDistinct_UOF$Incident_Type)

ggplot(SeattleDistinct_UOF, aes(Incident_Type))+
  geom_bar()


#Indianapolis Key: 1= physical, 2= Less than lethal weapon, 3= Lethal
IndianapolisDistinct_UOF$officerForceType<- gsub('Physical-Kick|Physical-Hands, Fist, Feet|Physical-Weight Leverage|Physical-Take Down|Physical-Palm Strike|Physical-Elbow Strike|Physical-Handcuffing|Physical-Leg Sweep|Physical-Knee Strike|Physical-Push|Physical-Other|Physical-Joint/Pressure|Physical-Fist Strike','1',IndianapolisDistinct_UOF$officerForceType)
IndianapolisDistinct_UOF$officerForceType<- gsub('Less Lethal-Taser|Less Lethal-Personal CS/OC spray|Less Lethal-Baton|Less Lethal-Burning CS|Less Lethal-Flash Bang|Less Lethal-Pepperball|Less Lethal-Bps Gas|Less Lethal-CS Grenade|Less Lethal-Other|Less Lethal-CS/OC|Less Lethal-Clearout OC|Less Lethal-Bean Bag|Less Lethal-CS Fogger|Canine Bite','2',IndianapolisDistinct_UOF$officerForceType)
IndianapolisDistinct_UOF$officerForceType<- gsub('Lethal-Handgun|Lethal-Vehicle','3',IndianapolisDistinct_UOF$officerForceType)
IndianapolisDistinct_UOF$officerForceType<- gsub('N/A',NA,IndianapolisDistinct_UOF$officerForceType)

ggplot(IndianapolisDistinct_UOF, aes(officerForceType))+
  geom_bar()

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

Orlando_UOF$UOF.Level<- gsub('3;4|4;3|4;4;3|4;3;4|4;4;4|4;4|4;4;3;3|4;3;3|4;4;3;4|4;4;3;4;3|4;4;4;3|3;3;4|3;4;4|4;3;4;3|4;3;4;3;4|3;4;3|4;4;3;4;3;4|3;4;3;4|4;4;3;3;4', '4', Orlando_UOF$UOF.Level)
Orlando_UOF$UOF.Level<- gsub('3;3','3',Orlando_UOF$UOF.Level)
Orlando_UOF[Orlando_UOF==""]<-NA


ggplot(Orlando_UOF, aes(UOF.Level))+
  geom_bar()



#Graphs compairing UOF level and Groupsize
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

