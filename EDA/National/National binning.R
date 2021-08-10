#National binning

library (dplyr)
library (tidyr)
library(stringr)
library(tidyverse)
library(here)
install.packages("data.table")
install.packages("Here")
library(data.table)
install.packages("epitools")
library(epitools)


National<-read.csv(file = here('clean data/National/national_officer_name.csv'), stringsAsFactors = FALSE, header= TRUE)

View(National)


#Binning weapon, fleeing, threat level into 2 bins

#Weapon -> weapon and no weapon
#Weapon = 1, no weapon = 2
# Does vehicle = weapon? I am going to say yes for now but i can change this if neccesary. 

National$weapon_bin <- National$weapon

National$weapon_bin[str_detect(National$weapon_bin, "Gun")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Knife")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Sword")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Machete")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Weapon")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Ax")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Hachet")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Vehicle")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "Taser")] <- 1

#Smae but not capitalized
National$weapon_bin[str_detect(National$weapon_bin, "gun")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "knife")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "sword")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "machete")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "weapon")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "ax")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "hachet")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "vehicle")] <- 1
National$weapon_bin[str_detect(National$weapon_bin, "taser")] <- 1

National$weapon_bin[National$weapon_bin != 1 ] <- 2


#Armed. armed= 1, unarmed = 2
National$armed_bin[National$armed_status== "Allegedly Armed"] <- 1
National$armed_bin[National$armed_status== "Unarmed/Did Not Have Actual Weapon"] <- 2
National$armed_bin[National$armed_status== "Unclear"] <- NA


#fleeing -> fleeing and not fleeing. fleeing = 1, not fleeing = 2
National$fleeing_bin[National$fleeing== "Not fleeing"] <- 2
National$fleeing_bin[National$fleeing== "Foot"] <- 1
National$fleeing_bin[National$fleeing== "Car"] <- 1


#threat level -> attack and not attack. attack = 2, not attack = 1

National$threat_bin[National$threat_level== "Attack"] <- 2
National$threat_bin[National$threat_level != "Attack"] <- 1


#Officer group size
National$officer_group_size_bin[National$officer_group_size== 1] <- 0
National$officer_group_size_bin[National$officer_group_size == 2] <- 1
National$officer_group_size_bin[National$officer_group_size > 2] <- 2

#Doing the odds ratios

# Weapon and officer group size. 1= weapon, 2= no weapon or i don't think it counts (stick, rock, door knob, etc.)

weapon_group <-table(National$officer_group_size_bin, National$weapon_bin)
print(weapon_group)
weapon_group_OR<-oddsratio(weapon_group)
print(weapon_group_OR$measure)
print(weapon_group_OR$p.value)

# IM not positive that im interpreting this correctly but i think this means that groups of 2+ officers are significantly less likely to shoot people without weapons.


#Threat level odds ratio. 2 = attack. 1 = not attack
threat_group <-table(National$officer_group_size_bin, National$threat_bin)


print(threat_group)

threat_group_OR<-oddsratio(threat_group)

print(threat_group_OR$measure)
print(threat_group_OR$p.value)
#Officers in larger groups significantly less likely to kill someone if they werent attacking compared to officers in groups of 1 (and maybe 2 but i havnt don that yet). 
#The odds of being shot while attacking vs not attacking are significantly higher for more then 2 officers 
#AKA, 1 officer = more killings of non-attackers. 3+ officers = less killing of non attacker. Because single officer feels more threatened?


#fleeing - fleeing = 1, not fleeing = 2

fleeing_group <-table(National$officer_group_size_bin, National$fleeing_bin)
print(fleeing_group)

fleeing_group_OR<-oddsratio(fleeing_group)
print(fleeing_group_OR$measure)
#So no significant results for fleeing 




# Data visualization for reason_for_encounter and encounter_type
ggplot(National, aes(reason_for_encounter))+
  geom_bar() +
  coord_flip()
#Oh shit. That was not ideal
#So i can even see the situation, I want to turn everything that is repeated 2 times or less into NA but i am struggling with that
National_RFE_narrow <- National$reason_for_encounter[!unique(National$reason_for_encounter), ]

National_RFE_narrow$RFE <-National$reason_for_encounter[replace_na(unique(National$reason_for_encounter))]

View(National_RFE_narrow)
#hepl.


#trying with encounter_type
ggplot(National, aes(encounter_type))+
  geom_bar() +
  coord_flip()

#This looks more managable
#Im going to narrow this down a little. 

National$encounter_type[str_detect(National$encounter_type, "Domestic")] <- "Domestic Disturbance"
National$encounter_type[National$encounter_type == "None/Unknown" ] <- NA
National$encounter_type[National$encounter_type == "NA" ] <- NA
National$encounter_type[str_detect(National$encounter_type, "Person")] <- "Person with a Weapon"
National$encounter_type[str_detect(National$encounter_type, "Traffic")] <- "Traffic Stop/Other Non-Violent Offense"
National$encounter_type[str_detect(National$encounter_type, "Non-Violent")] <- "Traffic Stop/Other Non-Violent Offense"

ggplot(National, aes(encounter_type))+
  geom_bar() +
  coord_flip()

#To do:
#Bin into violent / non violent 
#This graph by subject_race
#EDA work for national 


