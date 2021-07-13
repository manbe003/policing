#libraries
library(here)
library(gtools)

#loading in datasets
UOF14<-read.csv(file=here('dirty data/Northampton/PDI Use of Force 2014 DONE.csv'), stringsAsFactors = FALSE)
UOF15<-read.csv(file=here('dirty data/Northampton/PDI Use of Force 2015 DONE.csv'), stringsAsFactors = FALSE)
UOF18<-read.csv(file=here('dirty data/Northampton/Use of Force 2018.csv'), stringsAsFactors = FALSE)
UOF19<-read.csv(file=here('dirty data/Northampton/Use of Force 2019 - incident level.csv'), stringsAsFactors = FALSE)

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

#making Level of force into the needed bins
UOF_ALL_FixLevels<- UOF_ALL_GenderFix
UOF_ALL_FixLevels$`PD Force Type`<- gsub("40 MM Displayed|Firearm Displayed|Arm Bar Take-down|40MM Aimed at Human|Arm Bar Take-Down, Forced in to Chair, Strikes to Face|Arm Bar Take-Down, Forced in to Cruiser|Arm Bar Take-Down, Pinned Against Couch|
                                          Arm Bar Take-Down, Wrist Lock|Armbar Takedown to Wristlock, Physical Force - Knee w/ Body Weight to Hold Down|Armbar Takedown, Physical Force|Armbar Takedown, Physical force/subject resisted handcuffing|
                                          Arms Restrained, Carried to Wheelchair|Assisted with Restraining at CDH|Bent Wrist Lock|Body Drag|Brought to Cruiser in Escort Position|Carry/Drag|Double Arm Bar Take-Down|Double Arm Bar Take-Down, Moved in Escort Position
                                          Double Arm Bar Take-Down, Pressure Point & Arm Lock|Double Rear Wrist Lock, Restrained Against Wall|Double Rear Wrist Lock, Take Down|Double Take-Down|Escort Position, Arm Bar Take-Down|Escort to Cruiser|Escorted to Hood of Cruiser, Rear Wrist Lock|
                                          Escorted to Seated Position|Escorted to the Ground|Facial Strike|Foot Chase Tackle|Foot Chase, Pulled Down From Fence|Forced Against Bed, Wrist Lock|Forced Against Cruiser, Pulled Out of Car|Forced Against Wall|Forced in to Cruiser|Forced in to Escort Position|
                                          Forced Into Back Seat of Cruiser|Forced to Cruiser|Handcuffed hands in front, bodyweight to hold legs down|Handcuffs for Restraint|Jaw Hold to Prevent Swallowing|Knee Strike, RestrainedLimbs, Take Down|Knee Strikes|Knee to Shoulder Blade|Lifted and placed in back seat|
                                          Limb Restraint, Moved in Escort Position|Limb Restraint, Restrained on Couch|Locked feet together|Moved in Escort Position|Moved in Escort Position, Arm Bar Take-down|Moved in the Escort Position|Non-Compliant Cuffing|Non-Compliant Escort Position|
                                          Non-Compliant Escort Position, Armbar Takedown|Non-Compliant Escort Position, Front Wrist Lock|Non-Compliant Escort Position, Front Wrist Lock, Front Wrist Lock to Rear Wrist Lock, Bent Wrist Lock, Front Punch|Non-Compliant Escort Position, Front Wrist Lock, Front Wrist Lock to Rear Wrist Lock, Front Punch|
                                          Non-Compliant Escort Position, Physical Force|Non-Compliant Escort Position, Placed in prone position|Non-Compliant Escort Position, Rear Wrist Lock|Non-compliant Escort Position, Rear Wrist Lock, Bent Wrist Lock|Non-compliant Escort Position, Rear wrist Lock, Physical Force|Non-Compliant Escort Position, Rear wrist Lock, Physical Force|
                                          Non-Compliant Escort Position, Restraint hold of arms, legs and head|Non-Compliant Escort Position,Front Wrist Lock, Rear Wrist Lock|Non-Compliant Escort Position,Physically restrained subject's face so he could no longer yell / spit in officer's direction,, Rear Wrist Lock,, Double Leg Takedown|
                                          Non-Compliant Escort Position,Physically restrained subject's face so he could no longer yell / spit in officer's direction,, Rear Wrist Lock,, Double Leg Takedown|Non-Compliant Escort Technique|Non-Compliant, Rear Wrist Lock|Outside the Thigh|Physcial Force|Physical - Arm Bar Take-Down|Physical - Knee Strikes |
                                          Physical Force, Firearm Displayed|Physical Force, Non-Compliant Escort Position, Rear Wrist Lock, Armbar Takedown|Physical Force, Rear wrist Lock|Physical Force/Tackle|Physically pushed back|Pinned Against Cruiser|Pinned Against Stockade to Immobilize|Pinned Against Wall, Arm Bar Take-Down|Pinned and Restrained to Hospital Bed|
                                          Placed on the Ground, Carried to Cruiser|Positioned Head to Prevent Spitting|Pressure to Calf|Prone Position Restraint|Pulled and Pushed to Ground to Handcuff|Push, Physical Force|Push, Rear Wrist Lock |Rear Arm Lock Escort|Rear twist lock, Physical force |Rear Wrist Lock|Rear Wrist Lock,  Double Arm Bar Take-Down|Rear Wrist Lock, Arm Bar Take-Down|
                                          Rear Wrist Lock, Arm Drag|Rear Wrist Lock, Armbar Takedown, Armbar Takedown to Wristlock|Rear Wrist Lock, Baton Used to Leverage Subject's Right Arm from Underneath Body|Rear Wrist Lock, Holding Calf/Foot Down W/ Hand/Foot|Rear wrist lock, Physical force|Rear Wrist Lock, Physical Force|Rear Wrist Lock, Physical Force to Take Subject to the Ground|
                                          Rear Wrist Lock, Physical Force/Restraint|Rear Wrist Lock, Pinned Against Cruiser|Rear Wrist Lock, Pinned Against Wall|Rear Wrist Lock, Restrained with Forearm|Rear Wrist Lock, Tackle, Physical Force, Front Punch|Rear Wrsit Lock, Physcial Force|Restrained Legs/Lower BodyRestrained to Hospital Bed|Restrained to the Ground|Reverse Wrist Lock|Straddled Legs/ Held Them, knee strike, Leg swipe, front punch, knee strike|
                                          Strike to the Face|Strike to the Face, Restrained on Couch|Tackle|Tackled|Tackled When Fleeing, Limb Restraint, Knee to Back|Take Down|Take Down, Leg Restrained|Upward Hand Strike, Moved in Escort Position|Wrist Lock|Wrist Lock, Pushed Against Wall|Wrist Lock, Rear Wrist Lock, Knee Strike|Wrist Lock, Rear Wrist Lock, Knee Strike", "1", UOF_ALL_FixLevels$`PD Force Type`)





UOF_ALL_FixLevels$`PD Force Type`<- gsub("Baton|Baton - Forward Strikes|Baton - Front Jab|Baton - Pinned Legs|Baton - Leverage Tool|Baton - Rear Baton Arm Lock|Baton - Rear Baton Arm Lock, Forward Jab|Baton - Rear Baton Arm Lock, Forward Jab|Baton - Used as Leverage Tool|
                                          Baton - Used to Apply Pressure|Baton Compliance/ Distraction Technique|Baton Leg Lock|Baton, Leverage Pressure Compliance|Baton, Middle block|Emergency Restraint Chair|Forward Baton Strike|Non-compliant Escort Position, Front Wrist Lock, Rear Wrist Lock, Front Wrist Lock to Rear Wrist Lock, Front Punch, Baton|
                                          O.C Spray|O.C. Spray, Non-Compliant Escort Position, Physcial Force |O.C. Spray, Non-compliant Escort Position, Rear Wrist Lock |O.C. Spray, Non-Compliant Escort Position, Rear Wrist Lock, Front Wrist Lock to Rear Wrist Lock|O.C. Spray, Physical Force|O.C. Spray, Rear Wrist Lock|O.C. Spray, Rear Wrist Lock, Bent Wrist Lock, Physical Force|O.C. Spray, Roll the Ball Technique|
                                          O.C. Spray, Tackle|OC Spray|Physical Force, Baton|Picked Female Up, Restrained to Hospital Bed|Pinned and Restrained to Hospital Bed|Restrained Against Hopsital Bed|Restrained Left Wrist With 2 Hands to Remove Knife, Baton Used to Place Pressure on Hands to Release Knife|Used Baton to Place Pressure Across Achilles Tendon",  "2", UOF_ALL_FixLevels$`PD Force Type`)


UOF_ALL_FixLevels$`PD Force Type`<- gsub("40MM Fired at Human, OC Spray, Tackle Physcial Force|40MM Fired at Human|Firearm- Point/Aim at Human|Firearm - Point / Aim|Firearm displayed and pointed at human|Firearm Displayed, Pointed at Human|Firearm displayed, Pointed at Human, Physcial Force|Rear Wrist Lock, Firearm Displayed, Pointed at Human", "3", UOF_ALL_FixLevels$`PD Force Type`)






