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


#Function neccesary for bootstrapping 
add_zero_row <- function(race1, race2, dataset){
  Var1 <- race1
  Var2 <- race2
  Freq <- 0
  zerorow <- data.frame(Var1, Var2, Freq)
  validation = subset.data.frame(dataset,Var1 == race1 & Var2 == race2)
  if (nrow(validation) == 0){
    dataset <- rbind(dataset, zerorow)
  } else {
    dataset <- dataset
  }
}

#Bootstrapping - i'm using LouisvilleStops at the moment. Not sure if this is the best choice

LS_race<-subset(LouisvilleStops, select = "officer_race")

Var1 <- "stop"
Var2 <- "stop"
Freq <- "stop"
stop <- data.frame(Var1, Var2, Freq)

y  <- NULL;
for (i in 1:1000) {
  LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 15, replace = TRUE), ])
  colnames(LS_bootstrap) <- c("off1", "off2")
  LS_bootstrap <- as.data.frame(LS_bootstrap)
  LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2))
  LS_bootstrap_freqs <- add_zero_row("White", "Black", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Black", "White", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Hispanic", "White", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("White", "Hispanic", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Black", "Hispanic", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Hispanic", "Black", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("White", "Asian", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Asian", "White", LS_bootstrap_freqs)
  validation = subset.data.frame(LS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(LS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = LS_bootstrap_freqs[!(LS_bootstrap_freqs$Var1=="Black" & LS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    LS_bootstrap_freqs=total
  }
  #for whitehispanic
  validation = subset.data.frame(LS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(LS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
    total = LS_bootstrap_freqs[!(LS_bootstrap_freqs$Var1=="Hispanic" & LS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq + add$Freq
    LS_bootstrap_freqs=total
  }
  #for asianhispanic
  validation = subset.data.frame(LS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(LS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
    total = LS_bootstrap_freqs[!(LS_bootstrap_freqs$Var1=="Hispanic" & LS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq + add$Freq
    LS_bootstrap_freqs=total
  }
  #for blackhispanic
  validation = subset.data.frame(LS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Black')
  if (nrow(validation) != 0){
    add = subset.data.frame(LS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Black')
    total = LS_bootstrap_freqs[!(LS_bootstrap_freqs$Var1=="Hispanic" & LS_bootstrap_freqs$Var2=='Black'),]
    total[(total$Var1=='Black' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Black' & total$Var2=='Hispanic'),]$Freq + add$Freq
    LS_bootstrap_freqs=total
  }
  #for asianwhite
  validation = subset.data.frame(LS_bootstrap_freqs,Var1=='White' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(LS_bootstrap_freqs,Var1=='White' & Var2=='Asian')
    total = LS_bootstrap_freqs[!(LS_bootstrap_freqs$Var1=="White" & LS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='White'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='White'),]$Freq + add$Freq
    LS_bootstrap_freqs=total
  }
  y <- rbind(y, LS_bootstrap_freqs, stop)
}
colnames(y) <- c("off1", "off2", "freq")
View(y)

#making individual frequency datasets for all of the race combinations
#whitewhite
whitewhite <- subset(y, y$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")
whitewhite$freq <- as.numeric(whitewhite$freq)

#whiteblack
whiteblack <- subset(y, y$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
whiteblack$freq <- as.numeric(whiteblack$freq)

#blackblack
blackblack <- subset(y, y$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")
blackblack$freq <- as.numeric(blackblack$freq)

#hispanichispanic
hispanichispanic <- subset(y, y$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")
hispanichispanic$freq <- as.numeric(hispanichispanic$freq)

#hispanicwhite 
hispanicwhite <- subset(y, y$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "Hispanic")
hispanicwhite$freq <- as.numeric(hispanicwhite$freq)

#hispanicasian
hispanicasian <- subset(y, y$off1 == "Asian")
hispanicasian <- subset(hispanicasian, hispanicasian$off2 == "Hispanic")
hispanicasian$freq <- as.numeric(hispanicasian$freq)

#blackhispanic
blackhispanic <- subset(y, y$off1 == "Black")
blackhispanic <- subset(blackhispanic, blackhispanic$off2 == "Hispanic")
blackhispanic$freq <- as.numeric(blackhispanic$freq)

#asianwhite
asianwhite <- subset(y, y$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite$freq <- as.numeric(asianwhite$freq)


View(whitewhite)
hist(whitewhite$freq)
hist(blackblack$freq)
hist(whiteblack$freq)
hist(hispanicwhite$freq)


#Calculating the actual frequencies for comparision
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

View(LS_group_freqs) #this is the actual data for the groups of 2 officers that ended up shooting people
#it consists of 1 group of 2 black officers, 3 groups of 1 black 1 white officer, and 11 groups of 2 white officers


#We can use the bootstrapping to figure out if this is random or not. 
median(whitewhite$freq) #bootstrap = 9, actual = 11
median(blackblack$freq) #bootstrap = 1, actual = 1
median(blackwhite$freq) #bootstrap = 2, actual = 3
median(hispanicwhite$freq) #bootstrap = 0, actual = 0 
median(hispanichispanic$freq)#bootstrap = 0, actual = 0
median(hispanicasian$freq)#bootstrap = 0, actual = 0
median(blackhispanic$freq)#bootstrap = 0, actual = 0
median(asianwhite$freq)#bootstrap = 0, actual = 0

# I also need to do some sort of analysis to see if this is statistically significant
View(whitewhite)

sd(whitewhite$freq)
#Well, according to my (AP statistics level) calculations, getting 11 or more for whitewhite had a 14.69% chance of occuring randomly.
#However, these calculations may be off. 
