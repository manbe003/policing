install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ggplot2)

testing <- read.csv(here("clean data", "seattle", "shootings_Seattle.csv"), stringsAsFactors = FALSE)

View(IndianapolisShootings)

#Need for bootstrapping - officer race data in shootings and officer race data in bigger dataset.Need group sizes or repeated variable to create it 

#have done/started: Dallas, Louisville, Austin
#Can bootstrap: Indianapolis, orlando, Seattle
#Does not have neccesary information: Bloomington, New Orleans, Northampton, Norwich



#Indianapolis bootstrapping: 
IndianapolisShootings <- read.csv(here("clean data", "Indianapolis", "OIS.csv"), stringsAsFactors = FALSE)
IndianapolisUOF <- read.csv(here("clean data", "Indianapolis", "UOF.csv"), stringsAsFactors = FALSE)

#adding officer group size column 
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}
IndianapolisShootings <- OfficerGroupSize(IndianapolisShootings, "id", IndianapolisShootings$id)

#Calculating the actual frequencies for comparision
IS_race2 <- subset(IndianapolisShootings, IndianapolisShootings$officer_group_size == 2)
x<-subset(IS_race2, select = c("id", "officerRace"))
x<- x %>%
  group_by(grp = str_c('Column', rep(1:2, length.out = n()))) %>%
  mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = officerRace) %>%
  select(-rn)

IS_race2 <- x
IS_race2[ ,"id"] <- list(NULL)
colnames(IS_race2) <- c("off1", "off2")
IS_group_freqs <- data.frame(table(IS_race2$off1, IS_race2$off2))

View(IS_group_freqs)
#This is 6 whitewhite, 1 blackblack, and 1 blackwhite

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

#The actual bootstrapping 
IS_race<-subset(IndianapolisUOF, select = c("officerRace","officerIdentifier"))
IS_race <- unique(IS_race)
IS_race<-subset(IS_race, select = "officerRace")
IS_race[IS_race == "N/A"] <- NA
IS_race[IS_race == "Other"] <- NA
IS_race <- na.omit(IS_race)

Var1 <- "stop"
Var2 <- "stop"
Freq <- "stop"
stop <- data.frame(Var1, Var2, Freq)

y  <- NULL;
for (i in 1:1000) {
  IS_bootstrap <- replicate(2, IS_race[sample(nrow(IS_race), 8, replace = TRUE), ])
  colnames(IS_bootstrap) <- c("off1", "off2")
  IS_bootstrap <- as.data.frame(IS_bootstrap)
  IS_bootstrap_freqs <- data.frame(table(IS_bootstrap$off1, IS_bootstrap$off2))
  IS_bootstrap_freqs <- add_zero_row("White", "Black", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("Black", "White", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("Hispanic", "White", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("White", "Hispanic", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("Black", "Hispanic", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("Hispanic", "Black", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("White", "Asian", IS_bootstrap_freqs)
  IS_bootstrap_freqs <- add_zero_row("Asian", "White", IS_bootstrap_freqs)
  validation = subset.data.frame(IS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(IS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = IS_bootstrap_freqs[!(IS_bootstrap_freqs$Var1=="Black" & IS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    IS_bootstrap_freqs=total
  }
  #for whitehispanic
  validation = subset.data.frame(IS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(IS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
    total = IS_bootstrap_freqs[!(IS_bootstrap_freqs$Var1=="Hispanic" & IS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq + add$Freq
    IS_bootstrap_freqs=total
  }
  #for asianhispanic
  validation = subset.data.frame(IS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(IS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
    total = IS_bootstrap_freqs[!(IS_bootstrap_freqs$Var1=="Hispanic" & IS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq + add$Freq
    IS_bootstrap_freqs=total
  }
  

  y <- rbind(y, IS_bootstrap_freqs, stop)
  
}
colnames(y) <- c("off1", "off2", "freq")
View(y)
IndianapolisBootstrap <- y
IndianapolisBootstrapUnique <- y

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


#making individual frequency datasets for all of the race combinations WITH THE UNIQUE DATA!!!
#whitewhite
whitewhite <- subset(IndianapolisBootstrapUnique, IndianapolisBootstrapUnique$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")
whitewhite$freq <- as.numeric(whitewhite$freq)

#whiteblack
whiteblack <- subset(IndianapolisBootstrapUnique, IndianapolisBootstrapUnique$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
whiteblack$freq <- as.numeric(whiteblack$freq)

#blackblack
blackblack <- subset(IndianapolisBootstrapUnique, IndianapolisBootstrapUnique$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")
blackblack$freq <- as.numeric(blackblack$freq)

#hispanichispanic
hispanichispanic <- subset(IndianapolisBootstrapUnique, IndianapolisBootstrapUnique$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")
hispanichispanic$freq <- as.numeric(hispanichispanic$freq)

#hispanicwhite 
hispanicwhite <- subset(IndianapolisBootstrapUnique, IndianapolisBootstrapUnique$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "Hispanic")
hispanicwhite$freq <- as.numeric(hispanicwhite$freq)

#hispanicasian
hispanicasian <- subset(IndianapolisBootstrapUnique, IndianapolisBootstrapUnique$off1 == "Asian")
hispanicasian <- subset(hispanicasian, hispanicasian$off2 == "Hispanic")
hispanicasian$freq <- as.numeric(hispanicasian$freq)


median(whitewhite$freq) #actual = 6, bootstrap = 6, unique = 5
median(blackblack$freq) #actual = 1, bootstrap = 0, unique = 0
median(blackwhite$freq) #actual = 1, bootstrap = 2, unique = 2
median(hispanicwhite$freq) #actual = 0, bootstrap = 0, unique = 0
median(hispanichispanic$freq) #actual = 0, bootstrap = 0, unique = 0
median(hispanicasian$freq) #actual = 0, bootstrap = 0, unique = 0


#This looks very similar. Bodes well for the police department i guess. 

