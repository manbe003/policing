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
DS_race<-na.omit(DS_race)
DS_bootstrap <- replicate(2, DS_race[sample(nrow(DS_race), 1000, replace = TRUE), ])
View(DS_bootstrap)

#If V1= white and V2= black, then make v1=black and v2= white
if (DS_bootstrap[,1] == "Black" && DS_bootstrap[,2] == "White" ) {
  
  DS_bootstrap[,1][DS_bootstrap[,1] == "Black"] <- "White"
  DS_bootstrap[,2][DS_bootstrap[,2] == "White"] <- "Black"

}

#make it such that each i identifies itself, i shows up in table; LS_bootstrap<-cbind(LS_bootstrap, i)
# or switch the columns to make consistent

#once completes a round, check to see if 0's exist and if not add them
# or add 0's until 1000 (or start with table full of 0's)

LS_race<-subset(LouisvilleShootings, select = "officer_race")

LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 15, replace = TRUE), ])
colnames(LS_bootstrap) <- c("off1", "off2")
LS_bootstrap <- as.data.frame(LS_bootstrap)
LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2))
View(LS_bootstrap_freqs)


y  <- NULL;
for (i in 1:1000) {
  LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 15, replace = TRUE), ])
  colnames(LS_bootstrap) <- c("off1", "off2")
  LS_bootstrap <- as.data.frame(LS_bootstrap)
  LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2))
  y <- rbind(y, LS_bootstrap_freqs)
}
View(y)

colnames(y) <- c("off1", "off2", "freq")

whitewhite <- subset(y, y$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")

whiteblack <- subset(y, y$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
blackwhite <- subset(y, y$off1 == "Black")
blackwhite <- subset(blackwhite, blackwhite$off2 == "White")
blackwhite <- rbind(blackwhite, whiteblack)

blackblack <- subset(y, y$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")

whiteasian <- subset(y, y$off1 == "White")
whiteasian <- subset(whiteasian, whiteasian$off2 == "Asian")
asianwhite <- subset(y, y$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite <- rbind(whiteasian, asianwhite)

#blackasian
whiteasian <- subset(y, y$off1 == "White")
whiteasian <- subset(whiteasian, whiteasian$off2 == "Asian")
asianwhite <- subset(y, y$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite <- rbind(whiteasian, asianwhite)

View(blackblack)

hist(whitewhite$freq)
hist(blackblack$freq)

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

View(LS_race2)




