#load dependencies and set working directory
setwd(here())
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()
library(sqldf)


#loading Libraries
Indi_UOF<-read.csv(file = 'clean data/Indianapolis/UOF.csv', stringsAsFactors = FALSE)

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


#making a new data frame for binning
Indi2<- sqldf("SELECT
      A.id,A.useOfForceReason,A.officerForceType,BinningNumberofOfficers
      FROM Indi_UOF as A
      JOIN UOF_OfficerCount as B
      ON A.id = B.id")


#creating a new column for justification coding
Indi2$Justification <- Indi2$useOfForceReason

#Indianapolis binning for justification
##Fleeing,Non-compliant, = 1 (justified in using lvl 1 force)
##Resisting Arrest,Combative suspect= 2 (justified in lvl 2 force)
## Assaulting Citizens, Assaulting Officers = 3 (justified lvl 3 force)
Indi2$Justification[Indi2$Justification=="Fleeing"]<- "1"
Indi2$Justification[Indi2$Justification=="Non-Compliant"]<- "1"
Indi2$Justification[Indi2$Justification=="Resisting Arrest"]<- "2"
Indi2$Justification[Indi2$Justification=="Combative Suspect"]<- "2"
Indi2$Justification[Indi2$Justification=="Assaulting Citizen(s)"]<- "3"
Indi2$Justification[Indi2$Justification=="Assaulting Officer(s)"]<- "3"
Indi2$Justification[Indi2$Justification=="Canine Incident"]<- NA
Indi2$Justification[Indi2$Justification=="N/A"]<- NA

#removing first column bc we don't need them anymore
Indi2 <- Indi2[-c(1,2)]


#table of number of officers vs level of Force broken into groups based on justification
table(Indi2) 

#splitting the data based on justification level
X <- split(Indi2, Indi2$Justification)
str(X)

#making them easily accessible & named data frames
Justif1<- X[["1"]]
Justif2<- X[["2"]]
Justif3<- X[["3"]]

#graphs based on Justification
Justif1Graph <- ggplot(Justif1,
                aes(x = BinningNumberofOfficers,
                fill = officerForceType,))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 1")+
  scale_y_continuous(name="Count", limits=c(0, 3000), breaks = waiver())


Justif2Graph <- ggplot(Justif2,
                aes(x = BinningNumberofOfficers,
                fill = officerForceType,))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 2")


Justif3Graph <- ggplot(Justif3,
                       aes(x = BinningNumberofOfficers,
                           fill = officerForceType,))+
  geom_bar(position = "dodge")+
  ggtitle("Justification Lvl 3")+
  scale_y_continuous(name="Count", limits=c(0, 3000), breaks = waiver())


#easily comparing graphs & tables w/ same y-axis marks
##this is showing that the general distribution is the same across Justification levels
##pairs respond the most across all justification levels with 3+ groups a close 2nd, even in lethal force justification encounters
print(Justif1Graph)
table(Justif1)
print(Justif2Graph)
table(Justif2)
print(Justif3Graph)
table(Justif3)


######Running Justification probabilities
Just1_Prob <- as.data.frame(table(Justif1$BinningNumberofOfficers))
Just1_Prob <- cbind(Just1_Prob, prop.table(Just1_Prob$Freq))
names(Just1_Prob) <- c("NumberofOfficers","Freq", "Probability")

Just2_Prob <- as.data.frame(table(Justif2$BinningNumberofOfficers))
Just2_Prob <- cbind(Just2_Prob, prop.table(Just2_Prob$Freq))
names(Just2_Prob) <- c("NumberofOfficers","Freq", "Probability")

Just3_Prob <- as.data.frame(table(Justif3$BinningNumberofOfficers))
Just3_Prob <- cbind(Just3_Prob, prop.table(Just3_Prob$Freq))
names(Just3_Prob) <- c("NumberofOfficers","Freq", "Probability")






##More Graphs, little less relevant
#general distribution of justification across cases
ggplot(Indi2, aes(Justification)) +
  geom_bar()

#general graph and table of officer force type vs justification
ggplot(Indi2,
       aes(x = officerForceType,
           fill = Justification))+
  geom_bar(position = "dodge")

##force type is Y of the table, justification is X of the table
table(Indi2$officerForceType,Indi2$Justification)

#general graph of number of officers vs use of force reason
ggplot(Indi2,
       aes(x = BinningNumberofOfficers,
           fill = Justification))+
  geom_bar(position = "dodge")

table(Indi2$BinningNumberofOfficers,Indi2$Justification)






