#load dependencies and set working directory
source("ProjectPackageManagement.R")
PackageDependency()
setwd(here())

library(epitools)
library(ggplot2)

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
dataset$percent.white[dataset$Number.of.Officers == 1] <- NA
dataset$percent.white[dataset$Number.of.Officers == 1] <- NA

#going to make a percent white rounded to the nearest 4th to make the OR easier??
#Binning into 0% = 0, 1%-25%, 26%-50% = 2, 50% , 51%-75%, 76%-99%, 100%
dataset$Binning.Percent.White[dataset$percent.white == 0] <- "0%"
dataset$Binning.Percent.White[dataset$percent.white >0 & dataset$percent.white <= 25] <- "1-25%"
dataset$Binning.Percent.White[dataset$percent.white >25 & dataset$percent.white < 50] <- "26-49%"
dataset$Binning.Percent.White[dataset$percent.white == 50] <- "50%"
dataset$Binning.Percent.White[dataset$percent.white >50 & dataset$percent.white <= 75] <- "51-75%"
dataset$Binning.Percent.White[dataset$percent.white >75 & dataset$percent.white <= 99] <- "76-99%"
dataset$Binning.Percent.White[dataset$percent.white == 100] <- "100%"



#Doing an OR to test this out 
OR_Table<-table(dataset$Binning.Percent.White, dataset$Force.Type)
OR_Table <- OR_Table[c(3,1:2,4:7),]
print(OR_Table)

###Odds Ratio
OR<-oddsratio(OR_Table)
#printing the outcome so its easier to read
print(OR$measure)
print(OR$p.value)



#graph comparing the variables

ggplot(dataset,
       aes(x = Binning.Percent.White,
           fill = as.character(Force.Type)))+
  geom_bar(position = "dodge")

