#loading libraries
library(here)
library(epitools)
library(tidyverse)
library(gplots)
library(graphics)

#loading datasets
UOF<-read.csv(file=here('clean data/Northampton/Northampton UOF.csv'), stringsAsFactors = FALSE)

UOF_All_FixLevels <- UOF
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(PD.Force.Type = case_when(
    str_detect(PD.Force.Type, "Fired at Human") ~ "3",
    str_detect(PD.Force.Type, "Baton") ~ "2",
    str_detect(PD.Force.Type, "O.C. Spray") ~ "2",
    str_detect(PD.Force.Type, "OC Spray") ~ "2",
    str_detect(PD.Force.Type, "O.C Spray") ~ "2",
    str_detect(PD.Force.Type, "Firearm - Point / Aim") ~ "1",
    str_detect(PD.Force.Type, "Firearm- Point / Aim") ~ "1",
    str_detect(PD.Force.Type, "Firearm- Point/Aim") ~ "1",
    str_detect(PD.Force.Type, "Display") ~ "1",
    str_detect(PD.Force.Type, "Displayed") ~ "1",
    str_detect(PD.Force.Type, "displayed") ~ "1",
    str_detect(PD.Force.Type, "Pointed Firearm at Human") ~ "1",
    str_detect(PD.Force.Type, "Pointed at Human") ~ "1",
    str_detect(PD.Force.Type, "Aimed at Human") ~ "1",
    str_detect(PD.Force.Type, "Displayed") ~ "1",
    str_detect(PD.Force.Type, "Arm Bar") ~ "1",
    str_detect(PD.Force.Type, "Take-down") ~ "1",
    str_detect(PD.Force.Type, "Lock") ~ "1",
    str_detect(PD.Force.Type, "Strikes") ~ "1",
    str_detect(PD.Force.Type, "Take Down") ~ "1",
    str_detect(PD.Force.Type, "Escorted") ~ "1",
    str_detect(PD.Force.Type, "Restrained") ~ "1",
    str_detect(PD.Force.Type, "Strike") ~ "1",
    str_detect(PD.Force.Type, "Restrained") ~ "1",
    str_detect(PD.Force.Type, "Restraint") ~ "1",
    str_detect(PD.Force.Type, "Cruiser") ~ "1",
    str_detect(PD.Force.Type, "Knee") ~ "1",
    str_detect(PD.Force.Type, "Tackled") ~ "1",
    str_detect(PD.Force.Type, "Tackle") ~ "1",
    str_detect(PD.Force.Type, "Take-Down") ~ "1",
    str_detect(PD.Force.Type, "Takedown") ~ "1",
    str_detect(PD.Force.Type, "Escort Position") ~ "1",
    str_detect(PD.Force.Type, "Non-Compliant") ~ "1",
    str_detect(PD.Force.Type, "Pushed") ~ "1",
    str_detect(PD.Force.Type, "Foot Chase") ~ "1",
    str_detect(PD.Force.Type, "Against") ~ "1",
    str_detect(PD.Force.Type, "Physical force") ~ "1",
    str_detect(PD.Force.Type, "Prevent") ~ "1",
    str_detect(PD.Force.Type, "Physical Force") ~ "1",
    str_detect(PD.Force.Type, "knee strike") ~ "1",
    str_detect(PD.Force.Type, "Pressure to Calf") ~ "1",
    str_detect(PD.Force.Type, "Outside the Thigh") ~ "1",
    str_detect(PD.Force.Type, "Body Drag") ~ "1",
    str_detect(PD.Force.Type, "Assisted with Restraining at CDH") ~ "1",
    str_detect(PD.Force.Type, "Physcial Force") ~ "1",
    str_detect(PD.Force.Type, "Handcuffed hands in front, bodyweight to hold legs down") ~ "1",
    str_detect(PD.Force.Type, "Physically pushed back") ~ "1",
    str_detect(PD.Force.Type, "Lifted and placed in back seat") ~ "1",
    str_detect(PD.Force.Type, "Carry/Drag") ~ "1",
    str_detect(PD.Force.Type, "Push") ~ "1",
    str_detect(PD.Force.Type, "Forced") ~ "1",
    str_detect(PD.Force.Type, "Grab") ~ "1",
    str_detect(PD.Force.Type, "punch") ~ "1",
    str_detect(PD.Force.Type, "wrist lock") ~ "1",
    str_detect(PD.Force.Type, "Phsyical force") ~ "1",
    str_detect(PD.Force.Type, "Punch") ~ "1",
    str_detect(PD.Force.Type, "Held down") ~ "1",
    TRUE ~ PD.Force.Type
  ))




probability<-function(df, colnum){
  x<-cbind(table(df[,colnum]))
  
  print("odds of escalating to force of 3")
  print(x[3,1]/sum(x[,1]))
  
  print("odds of escalating past force of 1")
  print(sum(x[2:3,1])/sum(x[,1]))
}

probability(UOF_All_FixLevels, 10)

UOF_FixLevels<-split(UOF_All_FixLevels, f=(UOF_All_FixLevels$Binning.Number.of.Officers))
probability(UOF_FixLevels$`1`,10)
probability(UOF_FixLevels$`2`,10)
probability(UOF_FixLevels$`3`,10)


#now looking to do a chi-square test to assess difference
#first make a table of number of officers by force type
dt <- table(UOF_All_FixLevels$Binning.Number.of.Officers,UOF_All_FixLevels$PD.Force.Type)

#Graph
balloonplot(t(dt), main ="PD.Force.Type", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dt, shade = TRUE, las=2,main = "PD.Force.Type")

#could do chi square of 1 vs 2 or 3 but can't do a 1 vs 3 chi square because table isn't populated enough. 
dt<-dt[,1:2]
mosaicplot(dt, shade = TRUE, las=2,main = "PD.Force.Type")
chi<-chisq.test(dt)
chi