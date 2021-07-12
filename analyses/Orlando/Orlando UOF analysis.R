library(here)
library(ggplot2)
library(tidyr)
library(dplyr)

#loading files
UOF <- read.csv(file = here("clean data/orlando/UOF (cleaned).csv"), stringsAsFactors = FALSE)

#deleting rows where number of races and ethnicities dont match up.
UOF_Offenders<- UOF[-c(3041, 4376, 5295, 1125, 3039, 5293, 4374), ]

#separating rows to be one offender per row
UOF_Offenders<- UOF_Offenders %>% separate_rows(Offenders.Race, Offenders.Sex, Offenders.Ethnicity, sep= ";")

RaceCounts <- as.data.frame(table(UOF_Offenders$Offenders.Race)) 

#dataframes with counts of UOF being used on each race and the fraction it is of the total number of each race in the dataset
ElectroncicDevice <- as.data.frame(with(UOF_Offenders, table(Offenders.Race, Electronic.Device.Used))) 
  ElectroncicDevice <- ElectroncicDevice[-c(1,2,3), ]
  ElectroncicDevice$fraction = ElectroncicDevice$Freq / RaceCounts$Freq
ChemicalAgent <- as.data.frame(with(UOF_Offenders, table(Offenders.Race, Chemical.Agent.Used)))
  ChemicalAgent <- ChemicalAgent[-c(1,2,3), ]
  ChemicalAgent$fraction = ChemicalAgent$Freq / RaceCounts$Freq
Tackle <- as.data.frame(with(UOF_Offenders, table(Offenders.Race, Tackle.Take.Down)))
  Tackle <- Tackle[-c(1,2,3), ]
  Tackle$fraction = Tackle$Freq / RaceCounts$Freq
ImpactWeapon <- as.data.frame(with(UOF_Offenders, table(Offenders.Race, Impact.Weapons.Used)))
  ImpactWeapon <- ImpactWeapon[-c(1,2,3), ]
  ImpactWeapon$fraction = ImpactWeapon$Freq / RaceCounts$Freq
PhysicalStrike<- as.data.frame(with(UOF_Offenders, table(Offenders.Race, Physical.Strikes.Made)))
  PhysicalStrike <- PhysicalStrike[-c(1,2,3), ]
  PhysicalStrike$fraction = PhysicalStrike$Freq / RaceCounts$Freq
DeflationDevice<- as.data.frame(with(UOF_Offenders, table(Offenders.Race, Deflation.Devices.Used)))
  DeflationDevice <- DeflationDevice[-c(1,2,3), ]
  DeflationDevice$fraction = DeflationDevice$Freq / RaceCounts$Freq
K9<- as.data.frame(with(UOF_Offenders, table(Offenders.Race, K9.Unit.Involved)))
  K9 <- K9[-c(1,2,3), ]
  K9$fraction = K9$Freq / RaceCounts$Freq

  
##putting all the frequencies into one dataframe
AllFrequncies <- data.frame(Offenders.Race=c("Asian","Black","White"),ElectroncicDevice$Freq,ChemicalAgent$Freq,Tackle$Freq,ImpactWeapon$Freq,PhysicalStrike$Freq,DeflationDevice$Freq,K9$Freq,ElectroncicDevice$fraction,ChemicalAgent$fraction,Tackle$fraction,ImpactWeapon$fraction,PhysicalStrike$fraction,DeflationDevice$fraction,K9$fraction)



#some graphs of this
ggplot(UOF_Offenders, aes(Offenders.Race)) +
  geom_bar()

ggplot(ElectroncicDevice, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

ggplot(ChemicalAgent, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

ggplot(Tackle, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

ggplot(ImpactWeapon, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

ggplot(PhysicalStrike, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

ggplot(K9, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

ggplot(DeflationDevice, aes(Offenders.Race, fraction)) +        
  geom_bar(stat = "identity", fill="blue")+     
  scale_y_continuous(labels = scales::percent)

AllFreqGraphs <- AllFrequncies %>% pivot_longer(names_to="UOF.Type", values_to="Frequency", ElectroncicDevice.fraction:K9.fraction)
ggplot(AllFreqGraphs, aes(UOF.Type, Frequency, fill = Offenders.Race)) + geom_col(position = "dodge")+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

AllCountsGraphs <- AllFrequncies %>% pivot_longer(names_to="UOF.Type", values_to="Counts", ElectroncicDevice.Freq:K9.Freq)
ggplot(AllCountsGraphs, aes(UOF.Type, Counts, fill = Offenders.Race)) + geom_col(position = "dodge")+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
