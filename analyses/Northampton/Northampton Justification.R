#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()


#loading Libraries
Northamp<- read.csv(file = 'clean data/Northampton/Northampton UOF.csv', stringsAsFactors = FALSE)

#This will be moved into the cleaning script during code review but has to live here for now
Northamp <- Northamp
Northamp <- Northamp %>%
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


#Binning indianaoplis Justification
Northamp2<- Northamp[,c("PD.Force.Type","Binning.Number.of.Officers","Subject.Weapon")]
names(Northamp2)[names(Northamp2) == 'Subject.Weapon'] <- 'Justification'
Northamp2$Justification<- gsub('No|No ','1',Northamp2$Justification)
Northamp2$Justification<- gsub('Bottle|Knives|Power Drill|BB gun|Metal Flashlight|Rock|2 Knives|Knife|Knife/ Razor Blades|Glass Pane|Sharp Object|Uncapped Syringe|Aluminum Baseball Bat|Boxcutter|Hypo-dermic Needle|Pellet Rifle|Airsoft Gun/Kinfe|Knife, Pellet Gun by his Side|Weapons of Opportunity','2',Northamp2$Justification)
Northamp2$Justification<- gsub('Bayonet|Firearm|Shotgun|Gun|Handgun','3',Northamp2$Justification)
Northamp2$Justification<- gsub('Yes|Possible 3',NA,Northamp2$Justification)


#graph and table of general justification and one graph comparing justification to force used
table(Northamp2) 

#splitting the data based on justification level
X <- split(Northamp2, Northamp2$Justification)
str(X)

#making them easily accessible & named data frames
Justif1<- X[["1"]]
Justif2<- X[["2"]]
Justif3<- X[["3"]]


#graphs based on Justification
Justif1Graph <- ggplot(Justif1,
                       aes(x = Binning.Number.of.Officers,
                           fill = as.factor(PD.Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 1")


Justif2Graph <- ggplot(Justif2,
                       aes(x = Binning.Number.of.Officers,
                           fill =  as.factor(PD.Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 2")


Justif3Graph <- ggplot(Justif3,
                       aes(x = Binning.Number.of.Officers,
                           fill =  as.factor(PD.Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 3")


#easily comparing graphs & tables w/ same y-axis marks
##pairs respond the most across all justification levels with 3+ groups a close 2nd, even in lethal force justification encounters
## not enough data to be solid, justf1 gives preliminary idea that in justif1 the "appropriate" amount of force is used
print(Justif1Graph)
table(Justif1)
print(Justif2Graph)
table(Justif2)
print(Justif3Graph)
table(Justif3)

######Running Justification probabilities
Just1_Prob <- as.data.frame(table(Justif1$Binning.Number.of.Officers))
Just1_Prob <- cbind(Just1_Prob, prop.table(Just1_Prob$Freq))
names(Just1_Prob) <- c("NumberofOfficers","Freq", "Probability")

Just2_Prob <- as.data.frame(table(Justif2$Binning.Number.of.Officers))
Just2_Prob <- cbind(Just2_Prob, prop.table(Just2_Prob$Freq))
names(Just2_Prob) <- c("NumberofOfficers","Freq", "Probability")

Just3_Prob <- as.data.frame(table(Justif3$Binning.Number.of.Officers))
Just3_Prob <- cbind(Just3_Prob, prop.table(Just3_Prob$Freq))
names(Just3_Prob) <- c("NumberofOfficers","Freq", "Probability")


#Function for chi square significance testing
ChiFunction = function(Justif, Chi, Chisq){ 
  Chi<-na.omit(table(Justif$Binning.Number.of.Officers,Justif$PD.Force.Type))
  chisq <- chisq.test(as.numeric(Chi))
  print(chisq)
  
  mosaicplot(Chi, shade = TRUE, las=2, main = "Force.Type")
}

ChiFunction(Justif1, Chi1, Chisq1)
ChiFunction(Justif2, Chi2, Chisq2)
ChiFunction(Justif3, Chi3, Chisq3)

#Significance test between Justification level and number of officers
Chi<-na.omit(table(Northamp2$Justification, Northamp2$Binning.Number.of.Officers))
print(Chi)
chisq <- chisq.test(as.numeric(Chi))
print(chisq)

mosaicplot(Chi, shade = TRUE, las=2, main = "Justification")

