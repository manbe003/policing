#Adding officer group size and percent white columns 


#formulas to replicate with other cities:
#If names and races are separated by commas in the same string (need library(stringr): 
dataset$officer_race <- trimws(dataset$officer_race)
dataset$officer_race <- stri_trans_totitle(dataset$officer_race)
dataset$officer_group_size <- str_count(dataset$officer_name, ',')    # counting commas
dataset$officer_group_size <- dataset$officer_group_size + 1    #adding one
dataset$white_officers <- str_count(dataset$officer_race, 'White')
dataset$percent_white <- dataset$white_officers / dataset$officer_group_size
dataset$percent_white <- dataset$percent_white * 100


#If each officer is on a different row. 
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}
    #example: LouisvilleShootings <- OfficerGroupSize(LouisvilleShootings, "PIU_number", LouisvilleShootings$PIU_number)
OfficerRaceGroup <- dataset %>%  
  group_by(PIU_number, officer_race) %>% 
  summarise(Freq = n()) 

OfficerRaceGroup <- subset(OfficerRaceGroup, officer_race == "White")
OfficerRaceGroup[,"officer_race"] <- list(NULL)
colnames(OfficerRaceGroup) <- c("PIU_number", "white_officers")
dataset <- merge(OfficerRaceGroup, dataset, by = "PIU_number", all.y = TRUE)
dataset$white_officers[is.na(dataset$white_officers)] <- 0
dataset$percent_white <- dataset$white_officers / dataset$officer_group_size
dataset$percent_white <- dataset$percent_white * 100
dataset$percent_white <- as.factor(dataset$percent_white)
datasetUnique<-subset(dataset, !duplicated(PIU_number))
ggplot(datasetUnique, aes(percent_white))+
  geom_bar()


#Doing it with real datasets:

#Orlando
OrlandoShootings<-read.csv(file = here('clean data/orlando/shooting (cleaned).csv'), stringsAsFactors = FALSE)
OrlandoUOF<-read.csv(file = here('clean data/orlando/UOF (cleaned).csv'), stringsAsFactors = FALSE)

#Orlando shootings
OrlandoShootings$White.Officers <- str_count(OrlandoShootings$Officer.Race, 'White')
OrlandoShootings$Percent.White <- OrlandoShootings$White.Officers / OrlandoShootings$Number.Of.Officers.Involved
OrlandoShootings$Percent.White <- OrlandoShootings$Percent.White * 100
OrlandoShootings$Percent.Bipoc <- (100 - OrlandoShootings$Percent.White)
OrlandoShootings$Percent.Bipoc <- round(OrlandoShootings$Percent.Bipoc, digits = 0)
OrlandoShootings$Percent.White <- round(OrlandoShootings$Percent.White, digits = 0)


#Orlando UOF
OrlandoUOF$White.Officers <- str_count(OrlandoUOF$Officers.Race, 'White')
OrlandoUOF$Percent.White <- OrlandoUOF$White.Officers / OrlandoUOF$Officers.Involved
OrlandoUOF$Percent.White <- OrlandoUOF$Percent.White * 100
OrlandoUOF$Percent.Bipoc <- (100 - OrlandoUOF$Percent.White)
OrlandoUOF$Percent.Bipoc <- round(OrlandoUOF$Percent.Bipoc, digits = 0)
OrlandoUOF$Percent.White <- round(OrlandoUOF$Percent.White, digits = 0)

#OrlandoUOF$Percent.White <- as.character(OrlandoUOF$Percent.White)

#Binning into 0% bipoc officer = 1, less than 50% = 2, more than 50% = 3
OrlandoUOF$Binning.Percent.Bipoc[OrlandoUOF$Percent.Bipoc > 50] <- 3
OrlandoUOF$Binning.Percent.Bipoc[OrlandoUOF$Percent.Bipoc < 50] <- 2
OrlandoUOF$Binning.Percent.Bipoc[OrlandoUOF$Percent.Bipoc == 0] <- 1

OrlandoShootings$Binning.Percent.Bipoc[OrlandoShootings$Percent.Bipoc > 50] <- 3
OrlandoShootings$Binning.Percent.Bipoc[OrlandoShootings$Percent.Bipoc < 50] <- 2
OrlandoShootings$Binning.Percent.Bipoc[OrlandoShootings$Percent.Bipoc == 0] <- 1

#get rid of 1 person groups
OrlandoShootings$Binning.Percent.Bipoc[OrlandoShootings$Number.Of.Officers.Involved == 1] <- NA
OrlandoUOF$Binning.Percent.Bipoc[OrlandoUOF$Officers.Involved == 1] <- NA

  
View(OrlandoUOF)
View(OrlandoShootings)


#Some EDA with new bins 

ggplot(data=subset(OrlandoUOF, !(OrlandoUOF$Offenders.Race!=c("White", "Black"))), 
       aes(x = Binning.Percent.Bipoc,
           fill = Offenders.Race))+
  geom_bar(position = "dodge")

ggplot(data=subset(OrlandoUOF, !(OrlandoUOF$Offenders.Race!=c("White", "Black"))), 
       aes(x = Binning.Percent.Bipoc,
           fill = Offenders.Race))+
  geom_bar(position = "fill")



ggplot(data=subset(OrlandoUOF), 
       aes(x = Binning.Percent.Bipoc,
           fill = UOF.Level))+
  geom_bar(position = "dodge")


#Not enough multiple person officer groups to use shootings data
ggplot(data=subset(OrlandoShootings), 
       aes(x = Binning.Percent.Bipoc,
           fill = Suspect.Race))+
  geom_bar(position = "dodge")


#odds ratio, which i did cause i was curious but someone should check this. 
#making a table of Bipoc bins and UOF level
Levels.Comp<-table(OrlandoUOF$UOF.Level,OrlandoUOF$Binning.Percent.Bipoc)
Levels.Comp<-table(OrlandoUOF$Binning.Percent.Bipoc,OrlandoUOF$UOF.Level)
print(Levels.Comp)

#Odds Ratio of UOf level and bipoc bins
Comp.OR<-oddsratio(Levels.Comp)
print(Comp.OR$measure)
print(Comp.OR$p.value)
#Looks not significant, but idk what i am doing so this is probably wrong









#Seattle shootings 
SeattleShootings<-read.csv(file = here('clean data/seattle/shootings_Seattle.csv'), stringsAsFactors = FALSE)

OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "Officer.Group.Size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}
SeattleShootings <- OfficerGroupSize(SeattleShootings, "GO", SeattleShootings$GO)
OfficerRaceGroup <- SeattleShootings %>%  
  group_by(GO, Officer.Race) %>% 
  summarise(Freq = n()) 

OfficerRaceGroup <- subset(OfficerRaceGroup, Officer.Race == "White")
OfficerRaceGroup[,"Officer.Race"] <- list(NULL)
colnames(OfficerRaceGroup) <- c("GO", "white_officers")
SeattleShootings <- merge(OfficerRaceGroup, SeattleShootings, by = "GO", all.y = TRUE)
SeattleShootings$white_officers[is.na(SeattleShootings$white_officers)] <- 0
SeattleShootings$percent_white <- SeattleShootings$white_officers / SeattleShootings$Officer.Group.Size
SeattleShootings$percent_white <- SeattleShootings$percent_white * 100
SeattleShootings$percent_white <- as.character(SeattleShootings$percent_white)



#Let's work with Orlando UOF for now 
View(OrlandoUOF)
#Subset it to exclude rows where the officer group size is one
OrlandoGroupsUOF <- subset(OrlandoUOF, Officers.Involved != 1)

#A little EDA

ggplot(data=subset(OrlandoGroupsUOF, !(OrlandoGroupsUOF$Offenders.Race!=c("White", "Black"))), 
       aes(x = Percent.White,
           fill = Offenders.Race))+
  geom_bar(position = "fill")

#hmm so this is really confusing. This is consistent with our findings with the really bad data in national. 100% white groups actually seem to use force on Black people less then 0% white groups. 
#50% white groups and all white groups seem pretty similar. 

#time for odds ratios?




#Another analysis- this same thing but with subjects of only one race, then compare races. 
#So subset it to be only black, and see if 



