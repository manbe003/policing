#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())

#loading in datasets (UOF)
UOF14<-read.csv(file='dirty data/Northampton/PDI Use of Force 2014 DONE.csv', stringsAsFactors = FALSE)
UOF15<-read.csv(file='dirty data/Northampton/PDI Use of Force 2015 DONE.csv', stringsAsFactors = FALSE)
UOF16<-read.csv(file='dirty data/Northampton/PDI Use of Force 2016.csv', stringsAsFactors = FALSE)
UOF17<-read.csv(file='dirty data/Northampton/PDI Use of Force 2017.csv', stringsAsFactors = FALSE)
UOF18<-read.csv(file='dirty data/Northampton/Use of Force 2018.csv', stringsAsFactors = FALSE)
UOF19<-read.csv(file='dirty data/Northampton/Use of Force 2019 - incident level.csv', stringsAsFactors = FALSE)
UOF20<-read.csv(file='dirty data/Northampton/PDI 2020 - Use of Force (1).csv', stringsAsFactors = FALSE)


### UOF Dataset ###

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


#making a new column for binning Force type and binning force types into levels according to paper outline
UOF_All_FixLevels <- UOF_ALL_GroupFix
UOF_All_FixLevels$Force.Binning<- UOF_All_FixLevels$`PD Force Type`
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(Force.Binning = case_when(
    str_detect(Force.Binning, "Fired at Human") ~ "3",
    str_detect(Force.Binning, "Baton") ~ "2",
    str_detect(Force.Binning, "O.C. Spray") ~ "2",
    str_detect(Force.Binning, "OC Spray") ~ "2",
    str_detect(Force.Binning, "O.C Spray") ~ "2",
    str_detect(Force.Binning, "Firearm - Point / Aim") ~ "1",
    str_detect(Force.Binning, "Firearm- Point / Aim") ~ "1",
    str_detect(Force.Binning, "Firearm- Point/Aim") ~ "1",
    str_detect(Force.Binning, "Display") ~ "1",
    str_detect(Force.Binning, "Displayed") ~ "1",
    str_detect(Force.Binning, "displayed") ~ "1",
    str_detect(Force.Binning, "Pointed Firearm at Human") ~ "1",
    str_detect(Force.Binning, "Pointed at Human") ~ "1",
    str_detect(Force.Binning, "Aimed at Human") ~ "1",
    str_detect(Force.Binning, "Displayed") ~ "1",
    str_detect(Force.Binning, "Arm Bar") ~ "1",
    str_detect(Force.Binning, "Take-down") ~ "1",
    str_detect(Force.Binning, "Lock") ~ "1",
    str_detect(Force.Binning, "Strikes") ~ "1",
    str_detect(Force.Binning, "Take Down") ~ "1",
    str_detect(Force.Binning, "Escorted") ~ "1",
    str_detect(Force.Binning, "Restrained") ~ "1",
    str_detect(Force.Binning, "Strike") ~ "1",
    str_detect(Force.Binning, "Restrained") ~ "1",
    str_detect(Force.Binning, "Restraint") ~ "1",
    str_detect(Force.Binning, "Cruiser") ~ "1",
    str_detect(Force.Binning, "Knee") ~ "1",
    str_detect(Force.Binning, "Tackled") ~ "1",
    str_detect(Force.Binning, "Tackle") ~ "1",
    str_detect(Force.Binning, "Take-Down") ~ "1",
    str_detect(Force.Binning, "Takedown") ~ "1",
    str_detect(Force.Binning, "Escort Position") ~ "1",
    str_detect(Force.Binning, "Non-Compliant") ~ "1",
    str_detect(Force.Binning, "Pushed") ~ "1",
    str_detect(Force.Binning, "Foot Chase") ~ "1",
    str_detect(Force.Binning, "Against") ~ "1",
    str_detect(Force.Binning, "Physical force") ~ "1",
    str_detect(Force.Binning, "Prevent") ~ "1",
    str_detect(Force.Binning, "Physical Force") ~ "1",
    str_detect(Force.Binning, "knee strike") ~ "1",
    str_detect(Force.Binning, "Pressure to Calf") ~ "1",
    str_detect(Force.Binning, "Outside the Thigh") ~ "1",
    str_detect(Force.Binning, "Body Drag") ~ "1",
    str_detect(Force.Binning, "Assisted with Restraining at CDH") ~ "1",
    str_detect(Force.Binning, "Physcial Force") ~ "1",
    str_detect(Force.Binning, "Handcuffed hands in front, bodyweight to hold legs down") ~ "1",
    str_detect(Force.Binning, "Physically pushed back") ~ "1",
    str_detect(Force.Binning, "Lifted and placed in back seat") ~ "1",
    str_detect(Force.Binning, "Carry/Drag") ~ "1",
    str_detect(Force.Binning, "Push") ~ "1",
    str_detect(Force.Binning, "Forced") ~ "1",
    str_detect(Force.Binning, "Grab") ~ "1",
    str_detect(Force.Binning, "punch") ~ "1",
    str_detect(Force.Binning, "wrist lock") ~ "1",
    str_detect(Force.Binning, "Phsyical force") ~ "1",
    str_detect(Force.Binning, "Punch") ~ "1",
    str_detect(Force.Binning, "Held down") ~ "1",
    TRUE ~ Force.Binning
  ))



### Saving Cleaned Dataset ###
write.csv(UOF_All_FixLevels,"clean data/Northampton/Northampton UOF.csv",row.names = FALSE)
