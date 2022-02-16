#loading libraries
library(here)
library(epitools)
library(tidyverse)


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


UOF_All_FixLevels$wnw<-UOF_All_FixLevels$Subject.Race
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(`wnw` = case_when(
    str_detect(`wnw`, "Black") ~ "Nonwhite",
    str_detect(`wnw`, "White") ~ "White",
    str_detect(`wnw`, "Native") ~ "Nonwhite",
    str_detect(`wnw`, "Hispanic") ~ "Nonwhite",
    str_detect(`wnw`, "Asian") ~ "Nonwhite",
    str_detect(`wnw`, "Pacific") ~ "Nonwhite",
    str_detect(`wnw`, "B") ~ "Nonwhite",
    str_detect(`wnw`, "A") ~ "Nonwhite",
    str_detect(`wnw`, "Indian") ~ "Nonwhite",
    str_detect(`wnw`, "Middle") ~ "Nonwhite",
    str_detect(`wnw`, "W") ~ "White",
    TRUE ~ `wnw`
  ))

UOF_All_FixLevels$wnw=gsub("Unknown", NA, UOF_All_FixLevels$wnw)

OR_Prep = function(dataset,column){
  #making a column binning level of force as lethal vs non lethal
  dataset['Lethal.vs.Non-lethal.Weapon'] <- column
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('1|2', 'Non-Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('3', 'Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  
  #making a column binning level of force as Weapon vs No Weapon
  dataset['Weapon.vs.No Weapon'] <- column
  dataset$`Weapon.vs.No Weapon`<- gsub('2|3', 'Weapon', dataset$`Weapon.vs.No Weapon`)
  dataset$`Weapon.vs.No Weapon`<- gsub('1', 'No Weapon', dataset$`Weapon.vs.No Weapon`)
  return(dataset)
}

UOF_All_FixLevels <- OR_Prep(UOF_All_FixLevels,UOF_All_FixLevels$PD.Force.Type)

UOF_split<-split(UOF_All_FixLevels, UOF_All_FixLevels$wnw)

##Function doesnt work because it creates an Error when it sees there is a 0 in the Lethality table so doing it step by step
#making a table of Lethal vs non lethal used with each race
OR_Table<-table(UOF_split$Nonwhite$Binning.Number.of.Officers, UOF_split$Nonwhite$PD.Force.Type)
OR_Table <- OR_Table[ c(3,1:2),]
OR_Table <- OR_Table[- c(3),]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)

#making a table of Weapon vs No Weapon used with each race
OR_Table2<-table(UOF_All_FixLevels$Binning.Number.of.Officers, UOF_All_FixLevels$`Weapon.vs.No Weapon`)
OR_Table2 <- OR_Table2[c(3,1:2),]
OR_Table2<- OR_Table2[,c(2,1)]
print(OR_Table2)

###Odds Ratio
OR2<-oddsratio(OR_Table2)
#printing the outcome so its easier to read
print(OR2$measure)
print(OR2$p.value)



#graphs

ggplot(UOF_All_FixLevels,
       aes(x = Binning.Number.of.Officers,
           fill = as.character(PD.Force.Type)))+
  geom_bar(position = "dodge")

ggplot(UOF_All_FixLevels,
       aes(x = Binning.Number.of.Officers,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")

ggplot(UOF_All_FixLevels,
       aes(x = Binning.Number.of.Officers,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")


