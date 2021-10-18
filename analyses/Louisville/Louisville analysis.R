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

#the function to make an officer group size column
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}


LouisvilleShootings <- OfficerGroupSize(LouisvilleShootings, "PIU_number", LouisvilleShootings$PIU_number)
DallasShootings <- OfficerGroupSize(DallasShootings, "case", DallasShootings$case)


#Bootstrapping
LS_race<-subset(LouisvilleShootings, select = "officer_race")
LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 15, replace = TRUE), ])  #making 2 columns of 15 randomly selected races from Louisville shootings
colnames(LS_bootstrap) <- c("off1", "off2")
LS_bootstrap <- as.data.frame(LS_bootstrap)
LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2)) #calculating the frequences
View(LS_bootstrap_freqs)


#doing this 1000 times to get a ton of data, making it one big datasey
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


#making individual frequency datasets for all of the race combinations
#whitewhite
whitewhite <- subset(y, y$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")

#whiteblack
whiteblack <- subset(y, y$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
blackwhite <- subset(y, y$off1 == "Black")
blackwhite <- subset(blackwhite, blackwhite$off2 == "White")
blackwhite <- rbind(blackwhite, whiteblack)

#blackblack
blackblack <- subset(y, y$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")

#whiteasian
whiteasian <- subset(y, y$off1 == "White")
whiteasian <- subset(whiteasian, whiteasian$off2 == "Asian")
asianwhite <- subset(y, y$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite <- rbind(whiteasian, asianwhite)

#blackasian
blackasian <- subset(y, y$off1 == "Black")
blackasian <- subset(blackasian, blackasian$off2 == "Asian")
asianblack <- subset(y, y$off1 == "Asian")
asianblack <- subset(asianblack, asianblack$off2 == "Black")
asianblack <- rbind(blackasian, blackwhite)

#asianasian
asianasian <- subset(y, y$off1 == "Asian")
asianasian <- subset(blackblack, blackblack$off2 == "Asian")

View(whiteblack)

hist(whitewhite$freq)
hist(blackblack$freq)


#Calculating the actual frequencies for comparision
LS_race2 <- subset(LouisvilleShootings, LouisvilleShootings$officer_group_size == 2)
x<-subset(LS_race2, select = c("PIU_number", "officer_race"))
View(x)
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

View(LS_group_freqs) #this is the actual data for the groups of 2 officers that ended up shooting people
#it consists of 1 group of 2 black officers, 3 groups of 1 black 1 white officer, and 11 groups of 2 white officers

#We can use the bootstrapping to figure out if this is random or not. 

hist(whitewhite$freq)
hist(blackblack$freq)
hist(whiteblack$freq)
median(whitewhite$freq) #11 compared to 11
median(blackblack$freq) #0 compared to 1
median(whiteblack$freq) #2 compared to 3

#some problems: bootstrapping does not add up to 15 (help) and i need to know how to do more advanced statistics to figure out standard distrubitions, ect. 
#should i be taking the bootstrapping from Louisville shootings? Or a different dataset, maybe incidents? 


#OH the ones for whiteblack is only half! because its counting whiteblack and black white as 2 seperate things. Need to solve this. Can maybe use this:
#If V1= white and V2= black, then make v1=black and v2= white
for(i in 1:length(LS_bootstrap[,1])){
  if (LS_bootstrap[i,1] == "Black" && LS_bootstrap[i,2] == "White" ) {
    
    LS_bootstrap[i,1][LS_bootstrap[i,1] == "Black"] <- "White"
    LS_bootstrap[i,2][LS_bootstrap[i,2] == "White"] <- "Black"
    
  }}

#but will still only be half... help? 


