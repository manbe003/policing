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
LouisvilleStops<-read.csv(file = here('clean data/Louisville/LouisvilleStops.csv'), stringsAsFactors = FALSE)


#the function to make an officer group size column
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

LouisvilleShootings <- OfficerGroupSize(LouisvilleShootings, "PIU_number", LouisvilleShootings$PIU_number)



#Bootstrapping - i'm using LouisvilleStops at the moment. Not sure if this is the best choice

LS_race<-subset(LouisvilleStops, select = "officer_race")

y  <- NULL;
for (i in 1:1000) {
  LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 15, replace = TRUE), ])
  colnames(LS_bootstrap) <- c("off1", "off2")
  LS_bootstrap <- as.data.frame(LS_bootstrap)
  LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2))
  validation = subset.data.frame(LS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(LS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = LS_bootstrap_freqs[!(LS_bootstrap_freqs$Var1=="Black" & LS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    LS_bootstrap_freqs=total
  }
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

#blackblack
blackblack <- subset(y, y$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")

View(whitewhite)
hist(whitewhite$freq)
hist(blackblack$freq)
hist(whiteblack$freq)


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
median(whitewhite$freq) #9 compared to 11
median(blackblack$freq) #0 compared to 1
median(blackwhite$freq) #2 compared to 3

# A question still remains - why does it not add up to 15? Maybe because i am not counting asian/ hispanic people? seems like a problem. 
# I also need to do some sort of analysis to see if this is statistically significant