library(here)
library(epitools)
library(tidyverse)
library(dplyr)
library(tidyr)
library(sqldf)


UOF<- read.csv(file=here('clean data/Indianapolis/UOF.csv'), stringsAsFactors = FALSE)

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

#making a column binning number of officers
UOF_OfficerForce['Binning Number of Officers'] <- NA
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="1+"]<- NA
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="1"]<- "1"
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers=="2"]<- "2"
UOF_OfficerForce$`Binning Number of Officers`[UOF_OfficerForce$NumberofOfficers > 2]<- "3+"


probability<-function(df, colnum){
  x<-cbind(table(df[,colnum]))
  
  print("odds of escalating to force of 3")
  print(x[3,1]/sum(x[,1]))
  
  print("odds of escalating past force of 1")
  print(sum(x[2:3,1])/sum(x[,1]))
}

probability(UOF_OfficerForce, 2)

UOF_split<-split(UOF_OfficerForce, f=(UOF_OfficerForce$`Binning Number of Officers`))
probability(UOF_split$`1`,2)
probability(UOF_split$`2`,2)
probability(UOF_split$`3`,2)

#now looking to do a chi-square test to assess difference
#first make a table of number of officers by force type
dt <- table(UOF_OfficerForce$`Binning Number of Officers`,UOF_OfficerForce$MaxForce)

#Graph
balloonplot(t(dt), main ="IncidentType", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dt, shade = TRUE, las=2, main = "IncidentType")


#could do chi square of whole table, sort of...level 2 is a bit poor though

chi<-chisq.test(dt)
chi
