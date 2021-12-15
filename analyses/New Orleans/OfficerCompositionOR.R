#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())


#load datasets
NewORL<- read.csv(file = 'clean data/New Orleans/New Orleans UOF.csv', stringsAsFactors = F)


#Binning Force Type
dataset<- NewORL
dataset <- dataset %>%
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

#using Zoe's code to find percent white of officer groups
dataset$Officer.Race <- trimws(dataset$Officer.Race)
dataset$Officer.Race <- stri_trans_totitle(dataset$Officer.Race)
dataset$white.officers <- str_count(dataset$Officer.Race, 'White')
dataset$percent.white <- dataset$white.officers / dataset$Number.of.Officers
dataset$percent.white <- dataset$percent.white * 100
dataset$percent.white <- round(dataset$percent.white, digits = 0)

#get rid of 1 person groups
dataset1<-dataset
dataset1$percent.white[dataset1$Number.of.Officers == 1] <- NA
dataset1$percent.white[dataset1$Number.of.Officers == 1] <- NA

#Binning percentages to make the OR easier
#Binning into 0% = 0, 1%-25%, 26%-50% = 2, 50% , 51%-75%, 76%-99%, 100%
dataset1$Binning.Percent.White[dataset1$percent.white == 0] <- "0%"
dataset1$Binning.Percent.White[dataset1$percent.white >0 & dataset1$percent.white <= 25] <- "1-25%"
dataset1$Binning.Percent.White[dataset1$percent.white >25 & dataset1$percent.white < 50] <- "26-49%"
dataset1$Binning.Percent.White[dataset1$percent.white == 50] <- "50%"
dataset1$Binning.Percent.White[dataset1$percent.white >50 & dataset1$percent.white <= 75] <- "51-75%"
dataset1$Binning.Percent.White[dataset1$percent.white >75 & dataset1$percent.white <= 99] <- "76-99%"
dataset1$Binning.Percent.White[dataset1$percent.white == 100] <- "100%"


#doing OR prep making it less lethal vs lethal and weapon vs no weapon

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

dataset1 <- OR_Prep(dataset1,dataset1$Force.Type)


#only doing weapon/no weapon OR bc lethal/non lethal has to many 0s to compute
OR_Table<-table(dataset1$Binning.Percent.White, dataset1$`Weapon.vs.No Weapon`)
OR_Table <- OR_Table[c(3,1:2,4:7),]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)

  

#graph comparing the variables

ggplot(dataset1,
       aes(x = Binning.Percent.White,
           fill = as.character(Force.Type)))+
  geom_bar(position = "dodge")

ggplot(dataset1,
       aes(x = Binning.Percent.White,
           fill = `Lethal.vs.Non-lethal.Weapon`))+
  geom_bar(position = "dodge")


ggplot(dataset1,
       aes(x = Binning.Percent.White,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")


#doing the same thing but binning it as <50% and >50%
dataset2<- dataset1
dataset2$Binning.Percent.White[dataset2$percent.white < 50] <- "<50%"
dataset2$Binning.Percent.White[dataset2$percent.white == 50] <- "50%"
dataset2$Binning.Percent.White[dataset2$percent.white >50] <- ">50%"


#OR with new Binning
#only doing weapon OR because lethal OR has to many 0s to compute
  #making a table of Lethal vs non lethal used with each race
  OR_Table<-table(dataset2$Binning.Percent.White, dataset2$`Weapon.vs.No Weapon`)
  OR_Table <- OR_Table[c(3,1:2),]
  print(OR_Table)
  
  ###Odds Ratio
  OR<-oddsratio(OR_Table)
  #printing the outcome so its easier to read
  print(OR$measure)
  print(OR$p.value)
  


  
##Doing it again but binning it as 0%,1-99%, and 100% white groups
dataset3<- dataset
dataset3$Binning.Percent.White[dataset3$percent.white >=1 & dataset3$percent.white <=99 ] <- "1-99%"
dataset3$Binning.Percent.White[dataset3$percent.white == 0] <- "0%"
dataset3$Binning.Percent.White[dataset3$percent.white == 100] <- "100%"

dataset3 <- OR_Prep(dataset3,dataset3$Force.Type)

  
#making a table of Lethal vs non lethal used with each race
OR_Table<-table(dataset3$Binning.Percent.White, dataset3$`Weapon.vs.No Weapon`)
print(OR_Table)
  
###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)

#graphs
  
ggplot(dataset3,
       aes(x = Binning.Percent.White,
           fill = as.character(Force.Type)))+
  geom_bar(position = "dodge")


ggplot(dataset3,
       aes(x = Binning.Percent.White,
           fill = `Weapon.vs.No Weapon`))+
  geom_bar(position = "dodge")

  

