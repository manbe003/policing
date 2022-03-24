#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())


#load datasets
NewORL<- read.csv(file = 'clean data/New Orleans/New Orleans UOF.csv', stringsAsFactors = F)


#using Zoe's code to find percent white compsition of officer groups
NewORL$Officer.Race <- trimws(NewORL$Officer.Race)
NewORL$Officer.Race <- stri_trans_totitle(NewORL$Officer.Race)
NewORL$white.officers <- str_count(NewORL$Officer.Race, 'White')
NewORL$percent.white <- NewORL$white.officers / NewORL$Number.of.Officers
NewORL$percent.white <- NewORL$percent.white * 100
NewORL$percent.white <- round(NewORL$percent.white, digits = 0)

#get rid of 1 person "groups"
NewORL_Groups<-NewORL
NewORL_Groups$percent.white[NewORL_Groups$Number.of.Officers == 1] <- NA
NewORL_Groups$percent.white[NewORL_Groups$Number.of.Officers == 1] <- NA

#Binning percentages
#Binning into 0% = 0, 1%-25%, 26%-50% = 2, 50% , 51%-75%, 76%-99%, 100%
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white == 0] <- "0%"
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white >0 & NewORL_Groups$percent.white <= 25] <- "1-25%"
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white >25 & NewORL_Groups$percent.white < 50] <- "26-49%"
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white == 50] <- "50%"
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white >50 & NewORL_Groups$percent.white <= 75] <- "51-75%"
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white >75 & NewORL_Groups$percent.white <= 99] <- "76-99%"
NewORL_Groups$Binning.Percent.White[NewORL_Groups$percent.white == 100] <- "100%"


#doing OR prep making it weapon vs no weapon

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

NewORL_Groups <- OR_Prep(NewORL_Groups,NewORL_Groups$Force.Type.Binning)


#only doing weapon/no weapon OR bc lethal/non lethal has to many 0s to compute
OR_Table<-table(NewORL_Groups$Binning.Percent.White, NewORL_Groups$`Weapon.vs.No Weapon`)
OR_Table <- OR_Table[c(3,1:2,4:7),]
OR_Table <- OR_Table[, c(2,1)]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)

  

#graph comparing the variables

ggplot(NewORL_Groups,
       aes(x = Binning.Percent.White,
           fill = as.character(Force.Type.Binning)))+
  geom_bar(position = "dodge")

ggplot(NewORL_Groups,
       aes(x = Binning.Percent.White,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")


ggplot(NewORL_Groups,
       aes(x = Binning.Percent.White,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")


#doing the same thing but binning it as <50% and >50%
NewORL_Groups2<- NewORL_Groups
NewORL_Groups2$Binning.Percent.White[NewORL_Groups2$percent.white < 50] <- "<50%"
NewORL_Groups2$Binning.Percent.White[NewORL_Groups2$percent.white == 50] <- "50%"
NewORL_Groups2$Binning.Percent.White[NewORL_Groups2$percent.white >50] <- ">50%"


#OR with new Binning
#only doing weapon OR because lethal OR has to many 0s to compute
  #making a table of Lethal vs non lethal used with each race
  OR_Table<-table(NewORL_Groups2$Binning.Percent.White, NewORL_Groups2$`Weapon.vs.No Weapon`)
  OR_Table <- OR_Table[c(3,1:2),]
  print(OR_Table)
  
  ###Odds Ratio
  OR<-oddsratio(OR_Table)
  #printing the outcome so its easier to read
  print(OR$measure)
  print(OR$p.value)
  


  
##Doing it again but binning it as 0%,1-99%, and 100% white groups
NewORL_Groups3<- NewORL_Groups2
NewORL_Groups3$Binning.Percent.White[NewORL_Groups3$percent.white >=1 & NewORL_Groups3$percent.white <=99 ] <- "1-99%"
NewORL_Groups3$Binning.Percent.White[NewORL_Groups3$percent.white == 0] <- "0%"
NewORL_Groups3$Binning.Percent.White[NewORL_Groups3$percent.white == 100] <- "100%"
  
#making a table of weapon vs no weapon used with each race
OR_Table<-table(NewORL_Groups3$Binning.Percent.White, NewORL_Groups3$`Weapon.vs.No Weapon`)
OR_Table <- OR_Table[c(3,1:2),]
OR_Table <- OR_Table[, c(2,1)]
print(OR_Table)
  
###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)

#graphs
  
ggplot(NewORL_Groups3,
       aes(x = Binning.Percent.White,
           fill = as.character(Force.Type.Binning)))+
  geom_bar(position = "dodge")


ggplot(NewORL_Groups3,
       aes(x = Binning.Percent.White,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")

  

