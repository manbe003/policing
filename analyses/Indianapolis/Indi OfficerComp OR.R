#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())

#Load in dataset
UOF<- read.csv(file = 'clean data/Indianapolis/UOF.csv', stringsAsFactors = FALSE)

#Making a Force Binning column and binning the force type used
UOF$ForceBinning = UOF$officerForceType

UOF_FixLevels <- UOF
UOF_FixLevels <- UOF_FixLevels %>%
  mutate(ForceBinning = case_when(
    str_detect(ForceBinning, "Lethal") ~ "3",
    str_detect(ForceBinning, "Less Lethal") ~ "3",
    str_detect(ForceBinning, "Canine") ~ "2",
    str_detect(ForceBinning, "Physical") ~ "1",
    TRUE ~ ForceBinning
  ))

#making N/As in the column regular NAs
UOF_FixLevels$ForceBinning[UOF_FixLevels$ForceBinning=="N/A"]<-NA

#Counting the numbe of officers by counting the number of distinct Officer IDs with the same case ID and making a DF
UOF_OfficerCount <- sqldf("SELECT 
      id, COUNT(DISTINCT officerIdentifier)
      FROM UOF_FixLevels
      GROUP BY id")
colnames(UOF_OfficerCount)[2]<- "NumberofOfficers"


#Finding the Max force used in each case and making a DF
UOF_MaxForce<- sqldf("SELECT
      id, MAX(ForceBinning)
      FROM UOF_FixLevels
      GROUP BY id")
colnames(UOF_MaxForce)[2]<- "MaxForce"

#Combing the two DFs to make one with all the needed data
UOF_OfficerForce<- sqldf("SELECT
      A.id,A.MaxForce, NumberofOfficers
      FROM UOF_MaxForce as A
      JOIN UOF_OfficerCount as B
      ON A.id = B.id")

UOF2 <- rbind(UOF_FixLevels, UOF_OfficerForce)

#making a column binning number of officers
UOF_OfficerForce['Binning Number of Officers'] <- NA
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="1+"]<- NA
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="1"]<- "1"
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="2"]<- "2"
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers > 2]<- "3+"



#using Zoe's code to find percent white of officer groups
UOF_OfficerForce$officerRace <- trimws(UOF_OfficerForce$)
UOF_OfficerForce$officerRace <- stri_trans_totitle(UOF_OfficerForce$officerRace)
UOF_OfficerForce$white.officers <- str_count(UOF_OfficerForce$officerRace, 'White')
UOF_OfficerForce$percent.white <- UOF_OfficerForce$white.officers / UOF_OfficerForce$NumberofOfficers
UOF_OfficerForce$percent.white <- UOF_OfficerForce$percent.white * 100
UOF_OfficerForce$percent.white <- round(UOF_OfficerForce$percent.white, digits = 0)



