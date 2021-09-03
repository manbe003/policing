#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
PackageDependency()

#loading in datasets
UOF14<-read.csv(file='dirty data/Northampton/PDI Use of Force 2014 DONE.csv', stringsAsFactors = FALSE)
UOF15<-read.csv(file='dirty data/Northampton/PDI Use of Force 2015 DONE.csv', stringsAsFactors = FALSE)
UOF18<-read.csv(file='dirty data/Northampton/Use of Force 2018.csv', stringsAsFactors = FALSE)
UOF19<-read.csv(file='dirty data/Northampton/Use of Force 2019 - incident level.csv', stringsAsFactors = FALSE)

#renaming columns in each to what they should be named and removing the first row where the column names were
UOF14_FixCol<- UOF14
colnames(UOF14_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF14_FixCol<- UOF14_FixCol[-c(1), ]
UOF15_FixCol<- UOF15
colnames(UOF15_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF15_FixCol<- UOF15_FixCol[-c(1), ]
UOF18_FixCol<- UOF18
colnames(UOF18_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF18_FixCol<- UOF18_FixCol[-c(1), ]
UOF19_FixCol<- UOF19
colnames(UOF19_FixCol)<-(c("Event","Call Type","Time of Day","Subject Weapon","Subject Age","Subject Gender","Subject Race","Subject Ethnicity","Alcohol Drugs","PD Force Type","Number of Officers","Arrest or Protective Custody","Subject Injured","Officer Injured"))
UOF19_FixCol <- UOF19_FixCol[-c(1), ]
UOF19_FixCol <- subset( UOF19_FixCol, select = -15 )

#binding all datasets into one
UOF_ALL<-smartbind(UOF14_FixCol,UOF15_FixCol,UOF18_FixCol,UOF19_FixCol)

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
UOF_All_FixLevels <- UOF_ALL_GroupFix
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`PD Force Type` = case_when(
    str_detect(`PD Force Type`, "Fired at Human") ~ "3",
    str_detect(`PD Force Type`, "Pointed at Human") ~ "3",
    str_detect(`PD Force Type`, "Aimed at Human") ~ "3",
    str_detect(`PD Force Type`, "Firearm") ~ "3",
    str_detect(`PD Force Type`, "40MM") ~ "3",
    str_detect(`PD Force Type`, "Baton") ~ "2",
    str_detect(`PD Force Type`, "O.C. Spray") ~ "2",
    str_detect(`PD Force Type`, "OC Spray") ~ "2",
    str_detect(`PD Force Type`, "O.C Spray") ~ "2",
    str_detect(`PD Force Type`, "Displayed") ~ "1",
    str_detect(`PD Force Type`, "Arm Bar") ~ "1",
    str_detect(`PD Force Type`, "Take-down") ~ "1",
    str_detect(`PD Force Type`, "Lock") ~ "1",
    str_detect(`PD Force Type`, "Strikes") ~ "1",
    str_detect(`PD Force Type`, "Take Down") ~ "1",
    str_detect(`PD Force Type`, "Escorted") ~ "1",
    str_detect(`PD Force Type`, "Restrained") ~ "1",
    str_detect(`PD Force Type`, "Strike") ~ "1",
    str_detect(`PD Force Type`, "Restrained") ~ "1",
    str_detect(`PD Force Type`, "Restraint") ~ "1",
    str_detect(`PD Force Type`, "Cruiser") ~ "1",
    str_detect(`PD Force Type`, "Knee") ~ "1",
    str_detect(`PD Force Type`, "Tackled") ~ "1",
    str_detect(`PD Force Type`, "Tackle") ~ "1",
    str_detect(`PD Force Type`, "Take-Down") ~ "1",
    str_detect(`PD Force Type`, "Takedown") ~ "1",
    str_detect(`PD Force Type`, "Escort Position") ~ "1",
    str_detect(`PD Force Type`, "Non-Compliant") ~ "1",
    str_detect(`PD Force Type`, "Pushed") ~ "1",
    str_detect(`PD Force Type`, "Foot Chase") ~ "1",
    str_detect(`PD Force Type`, "Against") ~ "1",
    str_detect(`PD Force Type`, "Physical force") ~ "1",
    str_detect(`PD Force Type`, "Prevent") ~ "1",
    str_detect(`PD Force Type`, "Physical Force") ~ "1",
    str_detect(`PD Force Type`, "knee strike") ~ "1",
    str_detect(`PD Force Type`, "Pressure to Calf") ~ "1",
    str_detect(`PD Force Type`, "Outside the Thigh") ~ "1",
    str_detect(`PD Force Type`, "Body Drag") ~ "1",
    str_detect(`PD Force Type`, "Assisted with Restraining at CDH") ~ "1",
    str_detect(`PD Force Type`, "Physcial Force") ~ "1",
    str_detect(`PD Force Type`, "Handcuffed hands in front, bodyweight to hold legs down") ~ "1",
    str_detect(`PD Force Type`, "Physically pushed back") ~ "1",
    str_detect(`PD Force Type`, "Lifted and placed in back seat") ~ "1",
    str_detect(`PD Force Type`, "Carry/Drag") ~ "1",
    TRUE ~ `PD Force Type`
  ))

write.csv(UOF_All_FixLevels,"clean data/Northampton/Northampton UOF.csv",row.names = FALSE)