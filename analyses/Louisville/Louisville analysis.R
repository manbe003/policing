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
#Reading in datasets
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


#Function neccesary for bootstrapping. This checks to see if any race combinations are not represented, and, if so, replaces them a row with frequency zero
#The allows the median to be accurate and is essencial for future parts of the function
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

#i'm pulling the race list from LouisvilleStops
#I have no officer identifiable data that would allow me to unique the race lsit, so there are repeats.
LS_race<-subset(LouisvilleStops, select = "officer_race")

#creating a stop row that seperates simulated trials of 15 officer groups
Var1 <- "stop"
Var2 <- "stop"
Freq <- "stop"
stop <- data.frame(Var1, Var2, Freq)

#Running the bootstrapping.
y  <- NULL;
for (i in 1:10000) {
  LS_bootstrap <- replicate(2, LS_race[sample(nrow(LS_race), 15, replace = TRUE), ])
  colnames(LS_bootstrap) <- c("off1", "off2")
  LS_bootstrap <- as.data.frame(LS_bootstrap)
  LS_bootstrap_freqs <- data.frame(table(LS_bootstrap$off1, LS_bootstrap$off2))
  #Running a check on every racial group that adds a column with frequency of zero if it is not there
  LS_bootstrap_freqs <- add_zero_row("White", "Black", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Black", "White", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Hispanic", "White", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("White", "Hispanic", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Black", "Hispanic", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Hispanic", "Black", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("White", "Asian", LS_bootstrap_freqs)
  LS_bootstrap_freqs <- add_zero_row("Asian", "White", LS_bootstrap_freqs)
  #checking if black, white exists and if so, dumping it into white, black then deleting it to prevent future counting problems
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
#Adding column names
colnames(y) <- c("off1", "off2", "freq")

#Storing within named dataset 
#LouisvilleBootstrap <- y -- not in current use
LouisvilleBootstrap10000 <- y 

#making individual frequency datasets for all of the race combinations
#whitewhite
whitewhite <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")
whitewhite$freq <- as.numeric(whitewhite$freq)
#whiteblack
whiteblack <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
whiteblack$freq <- as.numeric(whiteblack$freq)
#blackblack
blackblack <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")
blackblack$freq <- as.numeric(blackblack$freq)
#hispanichispanic
hispanichispanic <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")
hispanichispanic$freq <- as.numeric(hispanichispanic$freq)
#hispanicwhite 
hispanicwhite <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "Hispanic")
hispanicwhite$freq <- as.numeric(hispanicwhite$freq)
#hispanicasian
hispanicasian <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "Asian")
hispanicasian <- subset(hispanicasian, hispanicasian$off2 == "Hispanic")
hispanicasian$freq <- as.numeric(hispanicasian$freq)
#blackhispanic
blackhispanic <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "Black")
blackhispanic <- subset(blackhispanic, blackhispanic$off2 == "Hispanic")
blackhispanic$freq <- as.numeric(blackhispanic$freq)
#asianwhite
asianwhite <- subset(LouisvilleBootstrap10000, LouisvilleBootstrap10000$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite$freq <- as.numeric(asianwhite$freq)

View(LouisvilleBootstrap10000)

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

#Taking the median of the bootstrapped datasets to find a comparable value 
median(whitewhite$freq) #bootstrap = 9, actual = 11, bootstrap10000 = 9
median(blackblack$freq) #bootstrap = 1, actual = 1, bootstrap10000 = 0
median(whiteblack$freq) #bootstrap = 2, actual = 3, bootstrap10000 = 5
median(hispanicwhite$freq) #bootstrap = 0, actual = 0, bootstrap10000 = 0
median(hispanichispanic$freq)#bootstrap = 0, actual = 0, bootstrap10000 = 0
median(hispanicasian$freq)#bootstrap = 0, actual = 0, bootstrap10000 = 0
median(blackhispanic$freq)#bootstrap = 0, actual = 0, bootstrap10000 = 0
median(asianwhite$freq)#bootstrap = 0, actual = 0, bootstrap10000 = 0

# I also need to do some sort of analysis to see if this is statistically significant 
View(whitewhite)
sd(whitewhite$freq)
#Well, according to my (AP statistics level) calculations, getting 11 or more for whitewhite had a 14.69% chance of occuring randomly.
#This is assuming that the dataset generated by bootstrapping represents a sampling distribution 
#There is a pretty big chance that i have messed up these calculations 
