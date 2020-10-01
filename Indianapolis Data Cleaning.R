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

#Make all null values  = N/A for OIS
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


table(OIS$officerIdentifier)


