#loading libraries
library(here)
library(epitools)
library(tidyverse)

#loading datasets
UOF<- read.csv(file=here('clean data/Norwich/norwich_UOF.csv'), stringsAsFactors = FALSE)

#Binning Force Type (1- no weapon, 2- non lethal weapon, 3- lethal force) 
##binning displaying a weapon as 1 (rather than 3 or 2)
UOF_All_FixLevels <- UOF
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`PD_force_type` = case_when(
    str_detect(`PD_force_type`, "Discharged") ~ "3",
    str_detect(`PD_force_type`, "Taser Deployment") ~ "2",
    str_detect(`PD_force_type`, "OC Spray") ~ "2",
    str_detect(`PD_force_type`, "O.C. Spray") ~ "2",
    str_detect(`PD_force_type`, "K9 bite") ~ "2",
    str_detect(`PD_force_type`, "Displayed") ~ "1",
    str_detect(`PD_force_type`, "displayed") ~ "1",
    str_detect(`PD_force_type`, "Forced to Ground") ~ "1",
    str_detect(`PD_force_type`, "Forced to ground") ~ "1",
    str_detect(`PD_force_type`, "Strike") ~ "1",
    str_detect(`PD_force_type`, "Escort position") ~ "1",
    str_detect(`PD_force_type`, "escort position") ~ "1",
    str_detect(`PD_force_type`, "Escort Position") ~ "1",
    str_detect(`PD_force_type`, "strike") ~ "1",
    str_detect(`PD_force_type`, "Physical Force") ~ "1",
    str_detect(`PD_force_type`, "Physical force") ~ "1",
    str_detect(`PD_force_type`, "Grapple/Wrestle") ~ "1",
    TRUE ~ `PD_force_type`
  ))



#Binning Number of Officers
UOF_All_FixLevels['Binning Number of Officers'] <- NA
UOF_All_FixLevels$`Binning Number of Officers`[UOF_All_FixLevels$number_of_officers=="1"]<- "1"
UOF_All_FixLevels$`Binning Number of Officers`[UOF_All_FixLevels$number_of_officers=="2"]<- "2"
UOF_All_FixLevels$`Binning Number of Officers`[UOF_All_FixLevels$number_of_officers > "2"]<- "3+"

probability<-function(df, colnum){
  x<-cbind(table(df[,colnum]))
  
  print("odds of escalating to force of 3")
  print(x[3,1]/sum(x[,1]))
  
  print("odds of escalating past force of 1")
  print(sum(x[2:3,1])/sum(x[,1]))
}

probability(UOF_All_FixLevels, 9)

UOF_FixLevels<-split(UOF_All_FixLevels, f=(UOF_All_FixLevels$`Binning Number of Officers`))
probability(UOF_FixLevels$`1`,9)
probability(UOF_FixLevels$`2`,9)
probability(UOF_FixLevels$`3`,9)


#now looking to do a chi-square test to assess difference
#first make a table of number of officers by force type
dt <- table(UOF_All_FixLevels$`Binning Number of Officers`,UOF_All_FixLevels$PD_force_type)
dt

#Graph
balloonplot(t(dt), main ="IncidentType", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dt, shade = TRUE, las=2, main = "IncidentType")

#not good but level 3 is particularly data poor
dt<-dt[,1:2]
mosaicplot(dt, shade = TRUE, las=2, main = "IncidentType")
chi<-chisq.test(dt)
chi
