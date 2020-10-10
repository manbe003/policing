#First I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)

#Set working directory
setwd("~/Desktop/GitAndR/Policing/dirty data/Indianapolis")

#I want to call in my datasets (OIS, UOF)
OIS<-read.csv(file = 'Indianapolis Officer Involved Shootings.csv', stringsAsFactors = FALSE)
UOF<-read.csv(file = 'Indianapolis Use of Force Incidents.csv', stringsAsFactors = FALSE)


#Match and replace for OIS
OIS$officerWeaponUsed[OIS$officerWeaponUsed=="IMPD - Duty Handgun"]<-("Duty Handgun")
OIS$officerWeaponUsed[OIS$officerWeaponUsed=="IMPD - Shotgun"]<-("Shotgun")
OIS$officerWeaponUsed[OIS$officerWeaponUsed=="IMPD - Patrol Rifle"]<-("Patrol Rifle")

#Make all null values = N/A for OIS
OIS$occurredDate[OIS$occurredDate==""]<-("N/A")
OIS$division[OIS$division==""]<-("N/A")
OIS$district[OIS$district==""]<-("N/A")
OIS$shift[OIS$shift==""]<-("N/A")
OIS$beat[OIS$beat==""]<-("N/A")
OIS$disposition[OIS$disposition==""]<-("N/A")
OIS$residentWeaponUsed[OIS$residentWeaponUsed==""]<-("N/A")
OIS$officerWeaponUsed[OIS$officerWeaponUsed==""]<-("N/A")
OIS$serviceType[OIS$serviceType==""]<-("N/A")
OIS$residentCondition[OIS$residentCondition==""]<-("N/A")
OIS$officerCondition[OIS$officerCondition==""]<-("N/A")
OIS$residentRace[OIS$residentRace==""]<-("N/A")
OIS$residentSex[OIS$residentSex==""]<-("N/A")
OIS$residentAge[OIS$residentAge==""]<-("N/A")
OIS$officerRace[OIS$officerRace==""]<-("N/A")
OIS$officerSex[OIS$officerSex==""]<-("N/A")
OIS$officerAge[OIS$officerAge==""]<-("N/A")
OIS$officerYearsOfService[OIS$officerYearsOfService==""]<-("N/A")
OIS$officerIdentifier[OIS$officerIdentifier==""]<-("N/A")
OIS$serviceType[OIS$serviceType=="Not Applicable"]<-("N/A")
OIS$residentRace[OIS$residentRace=="Unknown"]<-("N/A")


#Make all null values = N/A for UOF
UOF$id[UOF$id==""]<-("N/A")
UOF$occurredDate[UOF$occurredDate==""]<-("N/A")
UOF$division[UOF$division==""]<-("N/A")
UOF$district[UOF$district==""]<-("N/A")
UOF$shift[UOF$shift==""]<-("N/A")
UOF$beat[UOF$beat==""]<-("N/A")
UOF$useOfForceReason[UOF$useOfForceReason==""]<-("N/A")
UOF$officerForceType[UOF$officerForceType==""]<-("N/A")
UOF$disposition[UOF$disposition==""]<-("N/A")
UOF$serviceType[UOF$serviceType==""]<-("N/A")
UOF$arrestMade[UOF$arrestMade==""]<-("N/A")
UOF$arrestCharges[UOF$arrestCharges==""]<-("N/A")
UOF$residentInjured[UOF$residentInjured==""]<-("N/A")
UOF$residentHospitalized[UOF$residentHospitalized==""]<-("N/A")
UOF$residentCondition[UOF$residentCondition==""]<-("N/A")
UOF$officerInjured[UOF$officerInjured==""]<-("N/A")
UOF$officerHospitalized[UOF$officerHospitalized==""]<-("N/A")
UOF$officerCondition[UOF$officerCondition==""]<-("N/A")
UOF$residentRace[UOF$residentRace==""]<-("N/A")
UOF$residentSex[UOF$residentSex==""]<-("N/A")
UOF$residentAge[UOF$residentAge==""]<-("N/A")
UOF$officerRace[UOF$officerRace==""]<-("N/A")
UOF$officerSex[UOF$officerSex==""]<-("N/A")
UOF$officerAge[UOF$officerAge==""]<-("N/A")
UOF$officerYearsOfService[UOF$officerYearsOfService==""]<-("N/A")
UOF$officerIdentifier[UOF$officerIdentifier==""]<-("N/A")


table(UOF$id)


