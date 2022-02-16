#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()


#loading Libraries
NewORl_UOF<- read.csv(file = 'clean data/New Orleans/New Orleans UOF.csv', stringsAsFactors = FALSE)

#This will be moved into the cleaning script during code review but has to live here for now
NewOrl_UOF2 <- NewORl_UOF
NewOrl_UOF2 <- NewOrl_UOF2 %>%
  mutate(Force.Type = case_when(
    str_detect(Force.Type, "Discharged") ~ "3",
    str_detect(Force.Type, "Vehicle as Weapon") ~ "3",
    str_detect(Force.Type, "Escort Tech") ~ "2",
    str_detect(Force.Type, "CEW") ~ "2",
    str_detect(Force.Type, "Canine") ~ "2",
    str_detect(Force.Type, "Baton") ~ "2",
    str_detect(Force.Type, "NonTrad Impact Weapon") ~ "2",
    str_detect(Force.Type, "Pointed") ~ "1",
    str_detect(Force.Type, "Exhibited") ~ "1",
    str_detect(Force.Type, "Canine (No Bite)") ~ "1",
    str_detect(Force.Type, "Hands") ~ "1",
    str_detect(Force.Type, "Take Down") ~ "1",
    str_detect(Force.Type, "Takedown") ~ "1",
    str_detect(Force.Type, "Head Strike") ~ "1",
    str_detect(Force.Type, "Force") ~ "1",
    str_detect(Force.Type, "Handcuffed Subject") ~ "1",
    TRUE ~ Force.Type
  ))

#New Orleans justification binning, 
#1 = justified to use lvl 1 force, 2 = justified to use lvl 2 force, 3 = justified using lvl 3 force
NewOrl2<- NewOrl_UOF2[,c("Force.Type","Binning.Number.of.Officers","UOF.Reason")]
names(NewOrl2)[names(NewOrl2) == 'UOF.Reason'] <- 'Justification'
NewOrl2$Justification<- gsub('Flight from an Officer|Escape|refuse verbal commands|Room Clearing|Room CLearing|Building clearing|Room clearing|room clearing','1',NewOrl2$Justification)
NewOrl2$Justification<- gsub('Resisting Lawful Arrest|Possibly armed subject|Tactical Deployments|Felony Stop','2',NewOrl2$Justification)
NewOrl2$Justification<- gsub('Weapon Exhibited|Weapon Discharged|Battery on Police Officer|Battery on Reporting Person|Resisting Officer w/Weapon','3',NewOrl2$Justification)


#graph and table of general justification and one graph comparing justification to force used
table(NewOrl2) 

#splitting the data based on justification level
X <- split(NewOrl2, NewOrl2$Justification)
str(X)

#making them easily accessible & named data frames
Justif1<- X[["1"]]
Justif2<- X[["2"]]
Justif3<- X[["3"]]


#graphs based on Justification
Justif1Graph <- ggplot(Justif1,
                aes(x = Binning.Number.of.Officers,
                fill = as.factor(Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 1")+
  scale_y_continuous(name="Count", limits=c(0, 300), breaks = waiver())


Justif2Graph <- ggplot(Justif2,
                aes(x = Binning.Number.of.Officers,
                fill = as.factor(Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 2")+
  scale_y_continuous(name="Count", limits=c(0, 300), breaks = waiver())


Justif3Graph <- ggplot(Justif3,
                       aes(x = Binning.Number.of.Officers,
                           fill = as.factor(Force.Type),))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 3")+
  scale_y_continuous(name="Count", limits=c(0, 300), breaks = waiver())


#easily comparing graphs & tables w/ same y-axis marks
#at Justification 1 & 2 lvl 3 force is used most in each no matter the group size
#at Justification lvl 2 lvl 1 force trended to be used most across officer group sizes
##at Justification 3 more single and pairs of officers responded
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
  Chi<-na.omit(table(Justif$Binning.Number.of.Officers,Justif$Force.Type))
  chisq <- chisq.test(as.numeric(Chi))
  print(chisq)
  
  mosaicplot(Chi, shade = TRUE, las=2, main = "Force.Type")
}

ChiFunction(Justif1, Chi1, Chisq1)
ChiFunction(Justif2, Chi2, Chisq2)
ChiFunction(Justif3, Chi3, Chisq3)





#More Graphs, little less relevant
#general distribution of justification across cases
ggplot(NewOrl2, aes(Justification)) +
  geom_bar()

#general graph and table of officer force type vs justification
ggplot(NewOrl2,
       aes(x = Force.Type,
           fill = Justification))+
  geom_bar(position = "dodge")

##force type is Y of the table, justification is X of the table
table(NewOrl2$Force.Type,NewOrl2$Justification)

#general graph of number of officers vs use of force reason
ggplot(NewOrl2,
       aes(x = Binning.Number.of.Officers,
           fill = Justification))+
  geom_bar(position = "dodge")

###Number of officers is Y of the table, justification is X of the table
table(NewOrl2$Binning.Number.of.Officers,NewOrl2$Justification)
