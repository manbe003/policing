#Louisville analysis
#ideas
install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ggplot2)
LouisvilleShootings<-read.csv(file = here('clean data/Louisville/LouisvilleShootings.csv'), stringsAsFactors = FALSE)
DallasShootings<-read.csv(file = here('clean data/Dallas/Dallas_shootings.csv'), stringsAsFactors = FALSE)



View(LouisvilleShootings)

#the function
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}
View(LouisvilleShootings)
LouisvilleShootings <- OfficerGroupSize(LouisvilleShootings, "PIU_number", LouisvilleShootings$PIU_number)


DallasShootings <- OfficerGroupSize(DallasShootings, "case", DallasShootings$case)

ggplot(DallasShootingsUnique, aes(officer_group_size))+
  geom_bar()


#questions
#rare cases of multiple victims per one officer. Should i switch PUI_number with subject_name? what if the same person gets two different citations then? And there are a good deal of NAs in that column


#Ok. That worked. Now lets try to make a function for the % white/black column . This one will be more difficult. 

#This works, but there is probably a better way. Still it took me hours and I am quite proud of it.

OfficerRaceGroup <- LouisvilleShootings %>%  
  group_by(PIU_number, officer_race) %>% 
  summarise(Freq = n()) 

OfficerRaceGroup<-OfficerRaceGroup[sample_n(OfficerRaceGroup$officer_race=="White")]

OfficerRaceGroup[,"officer_race"] <- list(NULL)
colnames(OfficerRaceGroup) <- c("PIU_number", "white_officers")
LouisvilleShootings <- merge(OfficerRaceGroup, LouisvilleShootings, by = "PIU_number", all.y = TRUE)

LouisvilleShootings$white_officers[is.na(LouisvilleShootings$white_officers)] <- 0
LouisvilleShootings$percent_white <- LouisvilleShootings$white_officers / LouisvilleShootings$officer_group_size
LouisvilleShootings$percent_white <- LouisvilleShootings$percent_white * 100

LouisvilleShootings$percent_white <- as.factor(LouisvilleShootings$percent_white)

LouisvilleShootingsUnique<-subset(LouisvilleShootings, !duplicated(PIU_number))
View(LouisvilleShootingsUnique)

ggplot(LouisvilleShootingsUnique, aes(percent_white))+
  geom_bar()

#This works. Now lets try it with DallasShootings
OfficerRaceGroup <- DallasShootings %>%  
  group_by(case, officer_race) %>% 
  summarise(Freq = n()) 

OfficerRaceGroup<-OfficerRaceGroup[sample_n(OfficerRaceGroup$officer_race=="White")]

OfficerRaceGroup[,"officer_race"] <- list(NULL)
colnames(OfficerRaceGroup) <- c("case", "white_officers")
DallasShootings <- merge(OfficerRaceGroup, DallasShootings, by = "case", all.y = TRUE)

DallasShootings$white_officers[is.na(DallasShootings$white_officers)] <- 0
DallasShootings$percent_white <- DallasShootings$white_officers / DallasShootings$officer_group_size
DallasShootings$percent_white <- DallasShootings$percent_white * 100

DallasShootings$percent_white <- as.factor(DallasShootings$percent_white)

DallasShootingsUnique<-subset(DallasShootings, !duplicated(case))
View(DallasShootingsUnique)

ggplot(DallasShootingsUnique, aes(percent_white))+
  geom_bar()

ggplot(data=subset(DallasShootingsUnique, !(officer_group_size == 1)), aes(percent_white))+
  geom_bar()
  
ggplot(data=subset(DallasShootingsUnique, !(officer_group_size == 1)),
       aes(x = percent_white,
           fill = subject_race))+
  geom_bar(position = "dodge")

ggplot(DallasShootingsUnique,
       aes(x = percent_white,
           fill = subject_race))+
  geom_bar(position = "dodge")


#Bootstrapping
  

DS_race<-subset(DallasShootings, select = "officer_race")

DS_bootstrap <- replicate(2, DS_race[sample(nrow(DS_race), 1000, replace = TRUE), ])


LS_race<-subset(LouisvilleShootings, select = "officer_race")

LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 1000, replace = TRUE), ])
colnames(LS_bootstrap) <- c("off1", "off2")
LS_bootstrap <- as.data.frame(LS_bootstrap)

LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2))
View(LS_bootstrap_freqs)



LS_race2 <- subset(LouisvilleShootings, LouisvilleShootings$officer_group_size == 2)

x<-subset(LS_race2, select = c("PIU_number", "officer_race"))

x<- x %>%
  group_by(grp = str_c('Column', rep(1:2, length.out = n()))) %>%
  mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = officer_race) %>%
  select(-rn)

LS_race2 <- x
LS_race2[ ,"PIU_number"] <- list(NULL)
colnames(LS_race2) <- c("off1", "off2")
LS_group_freqs <- data.frame(table(LS_race2$off1, LS_race2$off2))

View(LS_group_freqs)



