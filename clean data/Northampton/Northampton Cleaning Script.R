#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
PackageDependency()

#loading in datasets
UOF14<-read.csv(file='dirty data/Northampton/PDI Use of Force 2014 DONE.csv', stringsAsFactors = FALSE)
UOF15<-read.csv(file='dirty data/Northampton/PDI Use of Force 2015 DONE.csv', stringsAsFactors = FALSE)
UOF16<-read.csv(file='dirty data/Northampton/PDI Use of Force 2016.csv', stringsAsFactors = FALSE)
UOF17<-read.csv(file='dirty data/Northampton/PDI Use of Force 2017.csv', stringsAsFactors = FALSE)
UOF18<-read.csv(file='dirty data/Northampton/Use of Force 2018.csv', stringsAsFactors = FALSE)
UOF19<-read.csv(file='dirty data/Northampton/Use of Force 2019 - incident level.csv', stringsAsFactors = FALSE)
UOF20<-read.csv(file='dirty data/Northampton/PDI 2020 - Use of Force (1).csv', stringsAsFactors = FALSE)



#renaming columns in each to what they should be named and removing the first row where the column names were
UOF14_FixCol<- UOF14
colnames(UOF14_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF14_FixCol<- UOF14_FixCol[-c(1), ]
UOF15_FixCol<- UOF15
colnames(UOF15_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF15_FixCol<- UOF15_FixCol[-c(1), ]
UOF16_FixCol<- UOF16
colnames(UOF16_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF16_FixCol<- UOF16_FixCol[-c(1), ]
UOF17_FixCol<- UOF17
colnames(UOF17_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF17_FixCol<- UOF17_FixCol[-c(1), ]
UOF18_FixCol<- UOF18
colnames(UOF18_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF18_FixCol<- UOF18_FixCol[-c(1), ]
UOF19_FixCol<- UOF19
colnames(UOF19_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF19_FixCol <- UOF19_FixCol[-c(1), ]
UOF19_FixCol <- subset( UOF19_FixCol, select = -15 )
UOF20_FixCol<- UOF20
colnames(UOF20_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF20_FixCol <- UOF20_FixCol[-c(1), ]
UOF20_FixCol <- subset( UOF20_FixCol, select = -15 )

#binding all datasets into one
UOF_ALL<-smartbind(UOF14_FixCol,UOF15_FixCol,UOF16_FixCol,UOF17_FixCol,UOF18_FixCol,UOF19_FixCol,UOF20_FixCol)

#making all Blank spaces to NA
UOF_ALL_NA <- UOF_ALL
UOF_ALL_NA[UOF_ALL_NA==""]<-NA

#making all M and F to male and female
UOF_ALL_GenderFix<- UOF_ALL_NA
UOF_ALL_GenderFix$`Subject Gender`[UOF_ALL_GenderFix$`Subject Gender`=="M"]<- "Male"
UOF_ALL_GenderFix$`Subject Gender`[UOF_ALL_GenderFix$`Subject Gender`=="F"]<- "Female"

#fixing the races and ethnicities so they are consistent
UOF_ALL_RaceFix<- UOF_ALL_GenderFix
UOF_ALL_RaceFix$`Subject Race`[UOF_ALL_RaceFix$`Subject Race`=="white"]<- "White"
UOF_ALL_RaceFix$`Subject Race`[UOF_ALL_RaceFix$`Subject Race`=="Black "]<- "Black"
UOF_ALL_RaceFix$`Subject Ethnicity`[UOF_ALL_RaceFix$`Subject Ethnicity`=="Non - Hisp."]<- "Non-Hisp."
UOF_ALL_RaceFix$`Subject Ethnicity`[UOF_ALL_RaceFix$`Subject Ethnicity`=="Non-His"]<- "Non-Hisp."

#binning officer group sizes according to paper notes (1 off = 0, 2 off = 1, 3+ off = 2)
UOF_ALL_GroupFix<- UOF_ALL_RaceFix
UOF_ALL_GroupFix['Binning Number of Officers'] <- NA
UOF_ALL_GroupFix$`Binning Number of Officers`[UOF_ALL_GroupFix$`Number of Officers`=="1+"]<- NA
UOF_ALL_GroupFix$`Binning Number of Officers`[UOF_ALL_GroupFix$`Number of Officers`=="1"]<- "1"
UOF_ALL_GroupFix$`Binning Number of Officers`[UOF_ALL_GroupFix$`Number of Officers`=="2"]<- "2"
UOF_ALL_GroupFix$`Binning Number of Officers`[UOF_ALL_GroupFix$`Number of Officers` > 2]<- "3+"



#Binning force types into levels according to paper outline

write.csv(UOF_ALL_GroupFix,"clean data/Northampton/Northampton UOF.csv",row.names = FALSE)
