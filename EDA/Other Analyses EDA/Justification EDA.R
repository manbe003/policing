#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()



#loading Libraries
Indi_UOF<-read.csv(file = 'clean data/Indianapolis/UOF.csv', stringsAsFactors = FALSE)
NewORl_UOF<- read.csv(file = 'clean data/New Orleans/New Orleans UOF.csv', stringsAsFactors = FALSE)

#Binning indianaoplis UOF type
Indi_UOF$officerForceType<- gsub('Physical-Kick|Physical-Hands, Fist, Feet|Physical-Weight Leverage|Physical-Take Down|Physical-Palm Strike|Physical-Elbow Strike|Physical-Handcuffing|Physical-Leg Sweep|Physical-Knee Strike|Physical-Push|Physical-Other|Physical-Joint/Pressure|Physical-Fist Strike','1',Indi_UOF$officerForceType)
Indi_UOF$officerForceType<- gsub('Less Lethal-Taser|Less Lethal-Personal CS/OC spray|Less Lethal-Baton|Less Lethal-Burning CS|Less Lethal-Flash Bang|Less Lethal-Pepperball|Less Lethal-Bps Gas|Less Lethal-CS Grenade|Less Lethal-Other|Less Lethal-CS/OC|Less Lethal-Clearout OC|Less Lethal-Bean Bag|Less Lethal-CS Fogger|Canine Bite','2',Indi_UOF$officerForceType)
Indi_UOF$officerForceType<- gsub('Lethal-Handgun|Lethal-Vehicle','3',Indi_UOF$officerForceType)
Indi_UOF$officerForceType<- gsub('N/A',NA,Indi_UOF$officerForceType)


#Counting the number of officers by counting the number of distinct Officer IDs with the same case ID and making a DF
UOF_OfficerCount <-Indi_UOF
UOF_OfficerCount <- sqldf("SELECT 
      id, COUNT(DISTINCT officerIdentifier)
      FROM Indi_UOF
      GROUP BY id")
colnames(UOF_OfficerCount)[2]<- "NumberofOfficers"

#making a column binning number of officers
UOF_OfficerCount['BinningNumberofOfficers'] <- NA
UOF_OfficerCount$BinningNumberofOfficers[UOF_OfficerCount$NumberofOfficers=="1+"]<- NA
UOF_OfficerCount$BinningNumberofOfficers[UOF_OfficerCount$NumberofOfficers=="1"]<- "1"
UOF_OfficerCount$BinningNumberofOfficers[UOF_OfficerCount$NumberofOfficers=="2"]<- "2"
UOF_OfficerCount$BinningNumberofOfficers[UOF_OfficerCount$NumberofOfficers > 2]<- "3+"


#making a new dataframe for binning
Indi_LessLethal<- sqldf("SELECT
      A.id,A.useOfForceReason,A.officerForceType,BinningNumbeofOfficers
      FROM Indi_UOF as A
      JOIN UOF_OfficerCount as B
      ON A.id = B.id")


#Indianapolis binning for justification for Less lethal force
##Fleeing = 1
##Non-compliant, Resisting Arrest = 2
## Combative suspect,canine Incident  = 3
## Assaulting Citizens, Assaulting Officers = 4
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Fleeing"]<- "1"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Non-Compliant"]<- "2"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Resisting Arrest"]<- "2"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Combative Suspect"]<- "3"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Assaulting Citizen(s)"]<- "4"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Assaulting Officer(s)"]<- "4"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="Canine Incident"]<- "3"
Indi_LessLethal$useOfForceReason[Indi_LessLethal$useOfForceReason=="N/A"]<- NA

Indi_LessLethal <- Indi_LessLethal[-c(1)]



#graph and table of general justification and one graph comparing justification to force used
Indi_LessLethTable <- table(Indi_LessLethal) 
print(Indi_LessLethTable)

ggplot(Indi_LessLethal, aes(useOfForceReason)) +
  geom_bar()

ggplot(Indi_LessLethal,
       aes(x = officerForceType,
           fill = useOfForceReason))+
  geom_bar(position = "dodge")

ggplot(Indi_LessLethal,
       aes(x = BinningNumbeofOfficers,
           fill = useOfForceReason))+
  geom_bar(position = "dodge")






##Indianapolis Lethal Force Binning


#Counting the number of officers by counting the number of distinct Officer IDs with the same case ID and making a DF
UOF_OfficerCount2 <-Indi_UOF
UOF_OfficerCount2 <- sqldf("SELECT 
      id, COUNT(DISTINCT officerIdentifier)
      FROM Indi_UOF
      GROUP BY id")
colnames(UOF_OfficerCount2)[2]<- "NumberofOfficers"

#making a column binning number of officers
UOF_OfficerCount2['BinningNumberofOfficers'] <- NA
UOF_OfficerCount2$BinningNumberofOfficers[UOF_OfficerCount2$NumberofOfficers=="1+"]<- NA
UOF_OfficerCount2$BinningNumberofOfficers[UOF_OfficerCount2$NumberofOfficers=="1"]<- "1"
UOF_OfficerCount2$BinningNumberofOfficers[UOF_OfficerCount2$NumberofOfficers=="2"]<- "2"
UOF_OfficerCount2$BinningNumberofOfficers[UOF_OfficerCount2$NumberofOfficers > 2]<- "3+"


#making a new dataframe for binning
Indi_Lethal<- sqldf("SELECT
      A.id,A.useOfForceReason,A.officerForceType,BinningNumberofOfficers
      FROM Indi_UOF as A
      JOIN UOF_OfficerCount2 as B
      ON A.id = B.id")

##Fleeing, non-compliant, Resisting Arrest, Combative Subject = 1
## Assaulting Citizens, Assaulting Officers = 2 
Indi_Lethal$useOfForceReason <- gsub('Fleeing|Non-Compliant|Resisting Arrest|Combative Suspect|Canine Incident', '1', Indi_Lethal$useOfForceReason)
Indi_Lethal$useOfForceReason[Indi_Lethal$useOfForceReason=="Assaulting Officer(s)"]<- "2"
Indi_Lethal$useOfForceReason[Indi_Lethal$useOfForceReason=="Assaulting Citizen(s)"]<- "2"
Indi_Lethal$useOfForceReason[Indi_Lethal$useOfForceReason=="N/A"]<- NA

Indi_Lethal <- Indi_Lethal[-c(1)]

#making a column binning level of force as lethal vs non lethal

Indi_Lethal$officerForceType<- gsub('1|2', 'Non-Lethal', Indi_Lethal$officerForceType)
Indi_Lethal$officerForceType<- gsub('3', 'Lethal', Indi_Lethal$officerForceType)




#graph and table of general justification and one graph comparing justification to force used
Indi_LethalTable <- table(Indi_Lethal) 
print(Indi_LethalTable)

ggplot(Indi_Lethal, aes(useOfForceReason)) +
  geom_bar()

ggplot(Indi_Lethal,
       aes(x = officerForceType,
           fill = useOfForceReason))+
  geom_bar(position = "dodge")

ggplot(Indi_Lethal,
       aes(x = BinningNumberofOfficers,
           fill = useOfForceReason))+
  geom_bar(position = "dodge")





### Doing the same thing for New Orleans

#New Orleans Less lethal justification binning
NewOrl_LessLethal<- NewORl_UOF[,c("UOF.Reason","Force.Type","Binning.Number.of.Officers")]
NewOrl_LessLethal$UOF.Reason<- gsub('Resisting Lawful Arrest|Room Clearing|Room CLearing|Building clearing|Room clearing|room clearing','1',NewOrl_LessLethal$UOF.Reason)
NewOrl_LessLethal$UOF.Reason<- gsub('refuse verbal commands|Possibly armed subject|Flight from an Officer|Escape|Tactical Deployments|Felony Stop','2',NewOrl_LessLethal$UOF.Reason)
NewOrl_LessLethal$UOF.Reason<- gsub('Weapon Exhibited|Weapon Discharged|Battery on Police Officer|Battery on Reporting Person|Resisting Officer w/Weapon','3',NewOrl_LessLethal$UOF.Reason)


#graph and table of general justification and one graph comparing justification to force used
NewOrl_LessLethaltable <- table(NewOrl_LessLethal) 
print(NewOrl_LessLethaltable)

ggplot(NewOrl_LessLethal, aes(UOF.Reason)) +
  geom_bar()

ggplot(NewOrl_LessLethal,
       aes(x = Force.Type,
           fill = UOF.Reason))+
  geom_bar(position = "dodge")

ggplot(NewOrl_LessLethal,
       aes(x = Binning.Number.of.Officers,
           fill = UOF.Reason))+
  geom_bar(position = "dodge")


#new orleans lethal dataset

NewOrl_Lethal<- NewORl_UOF[,c("UOF.Reason","Force.Type","Binning.Number.of.Officers")]
NewOrl_Lethal$UOF.Reason<- gsub('Resisting Lawful Arrest|Room Clearing|Room CLearing|Building clearing|Room clearing|room clearing|refuse verbal commands|Flight from an Officer|Escape|Tactical Deployments|Felony Stop','1',NewOrl_Lethal$UOF.Reason)
NewOrl_Lethal$UOF.Reason<- gsub('Possibly armed subject|Battery on Police Officer|Battery on Police Officer|Battery on Reporting Person','2',NewOrl_Lethal$UOF.Reason)
NewOrl_Lethal$UOF.Reason<- gsub('Weapon Exhibited|Weapon Discharged|Resisting Officer w/Weapon','3',NewOrl_Lethal$UOF.Reason)

#making a column binning level of force as lethal vs non lethal
NewOrl_Lethal$Force.Type<- gsub('1|2', 'Non-Lethal', NewOrl_Lethal$Force.Type)
NewOrl_Lethal$Force.Type<- gsub('3', 'Lethal', NewOrl_Lethal$Force.Type)



###graph and table of general justification and one graph comparing justification to force used
NewOrl_Lethaltable <- table(NewOrl_Lethal) 
print(NewOrl_Lethaltable)

ggplot(NewOrl_Lethal, aes(UOF.Reason)) +
  geom_bar()

ggplot(NewOrl_Lethal,
       aes(x = Force.Type,
           fill = UOF.Reason))+
  geom_bar(position = "dodge")

ggplot(NewOrl_Lethal,
       aes(x = Binning.Number.of.Officers,
           fill = UOF.Reason))+
  geom_bar(position = "dodge")

