#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())

#load datasets
UOF<- read.csv(file = 'clean data/orlando/UOF (cleaned).csv', stringsAsFactors = FALSE)

#using Zoe's code to find percent white of officer groups
UOF$Officers.Race <- trimws(UOF$Officers.Race)
UOF$Officers.Race <- stri_trans_totitle(UOF$Officers.Race)
UOF$white.officers <- str_count(UOF$Officers.Race, 'White')
UOF$percent.white <- UOF$white.officers / UOF$Officers.Involved
UOF$percent.white <- UOF$percent.white * 100
UOF$percent.white <- round(UOF$percent.white, digits = 0)

#deleting single officer cases
UOF2<-UOF
UOF2$percent.white[UOF2$Number.of.Officers == 1] <- NA

##Binning it as 0%,1-99%, and 100% white groups
UOF2$Binning.Percent.White[UOF2$percent.white >=1 & UOF2$percent.white <=99 ] <- "1-99%"
UOF2$Binning.Percent.White[UOF2$percent.white == 0] <- "0%"
UOF2$Binning.Percent.White[UOF2$percent.white == 100] <- "100%"


#function for splitting force into lethal vs non lethal and Weapon vs no Weapon
OR_Prep = function(dataset,column){
  #making a column binning level of force as lethal vs non lethal
  dataset['Lethal.vs.Non-lethal.Weapon'] <- column
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('1|2', 'Non-Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  dataset$`Lethal.vs.Non-lethal.Weapon`<- gsub('3', 'Lethal', dataset$`Lethal.vs.Non-lethal.Weapon`)
  
  #making a column binning level of force as Weapon vs No Weapon
  dataset['Weapon.vs.No Weapon'] <- column
  dataset$`Weapon.vs.No Weapon`<- gsub('2|3', 'Weapon', dataset$`Weapon.vs.No Weapon`)
  dataset$`Weapon.vs.No Weapon`<- gsub('1', 'No Weapon', dataset$`Weapon.vs.No Weapon`)
  return(dataset)
}

UOF2 <- OR_Prep(UOF2,UOF2$UOF.Level)


#Doing OR with Group compositions
#only doing weapon/no weapon OR bc lethal/non lethal has to many 0s to compute
OR_Table<-table(UOF2$Binning.Percent.White, UOF2$`Weapon.vs.No Weapon`)
OR_Table <- OR_Table[c(3,1:2),]
OR_Table <- OR_Table[, c(2,1)]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)


#graphs

ggplot(UOF2,
       aes(x = Binning.Percent.White,
           fill = as.character(UOF.Level)))+
  geom_bar(position = "dodge")


ggplot(dataset3,
       aes(x = Binning.Percent.White,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")



