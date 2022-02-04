#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()


#loading Libraries
Northamp<- read.csv(file = 'clean data/Northampton/Northampton UOF.csv', stringsAsFactors = FALSE)


#Binning indianaoplis UOF type
Northamp2<- Northamp[,c("PD.Force.Type","Binning.Number.of.Officers","Subject.Weapon")]
names(Northamp2)[names(Northamp2) == 'Subject.Weapon'] <- 'Justification'
Northamp2$Justification<- gsub('No','1',Northamp2$Justification)
Northamp2$Justification<- gsub('Bottle|Metal Flashlight|Rock|2 Knives|Knife|Knife/ Razor Blades|Glass Pane|Sharp Object|Uncapped Syringe|Aluminum Baseball Bat|Boxcutter|Hypo-dermic Needle|Pellet Rifle|Airsoft Gun/Kinfe|Knife, Pellet Gun by his Side|Weapons of Opportunity','2',Northamp2$Justification)
Northamp2$Justification<- gsub('Bayonet|Firearm','3',Northamp2$Justification)
Northamp2$Justification<- gsub('Yes',NA,Northamp2$Justification)


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
  ggtitle("Justification Lvl 2")+


Justif3Graph <- ggplot(Justif3,
                       aes(x = Binning.Number.of.Officers,
                           fill =  as.factor(PD.Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 3")+


#easily comparing graphs & tables w/ same y-axis marks
##pairs respond the most across all justification levels with 3+ groups a close 2nd, even in lethal force justification encounters
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


