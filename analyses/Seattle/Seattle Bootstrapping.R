#Seattle bootstrapping

install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ggplot2)

SeattleShootings <- read.csv(here("clean data", "Seattle", "shootings_Seattle.csv"), stringsAsFactors = FALSE)
SeattleCitations <- read.csv(here("clean data", "Seattle", "citations_Seattle.csv"), stringsAsFactors = FALSE)

View(SeattleShootings)
View(SeattleCitations)

OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

SeattleShootings <- OfficerGroupSize(SeattleShootings, "GO", SeattleShootings$GO)

#Calculating the actual frequencies for comparision
SS_race2 <- subset(SeattleShootings, SeattleShootings$officer_group_size == 2)
x<-subset(SS_race2, select = c("GO", "Officer.Race"))
x<- x %>%
  group_by(grp = str_c('Column', rep(1:2, length.out = n()))) %>%
  mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = Officer.Race) %>%
  select(-rn)

SS_race2 <- x
SS_race2[ ,"GO"] <- list(NULL)
colnames(SS_race2) <- c("off1", "off2")
SS_group_freqs <- data.frame(table(SS_race2$off1, SS_race2$off2))

View(SS_group_freqs)
#This is 13 total
#8 whitewhite, 2 asianwhite, 1 hispanicwhite, 2 blackwhite



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
SS_race<-subset(SeattleCitations, select = "Officer.Race")
SS_race[SS_race == "N/A"] <- NA
SS_race[SS_race == "Other"] <- NA
SS_race <- na.omit(SS_race)
SS_race[SS_race == "Black or African American"] <- "Black"


Var1 <- "stop"
Var2 <- "stop"
Freq <- "stop"
stop <- data.frame(Var1, Var2, Freq)

y  <- NULL;
for (i in 1:1000) {
  SS_bootstrap <- replicate(2, SS_race[sample(nrow(SS_race), 13, replace = TRUE), ])
  colnames(SS_bootstrap) <- c("off1", "off2")
  SS_bootstrap <- as.data.frame(SS_bootstrap)
  SS_bootstrap_freqs <- data.frame(table(SS_bootstrap$off1, SS_bootstrap$off2))
  SS_bootstrap_freqs <- add_zero_row("White", "Black", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("Black", "White", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("Hispanic", "White", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("White", "Hispanic", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("Black", "Hispanic", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("Hispanic", "Black", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("White", "Asian", SS_bootstrap_freqs)
  SS_bootstrap_freqs <- add_zero_row("Asian", "White", SS_bootstrap_freqs)
  validation = subset.data.frame(SS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(SS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = SS_bootstrap_freqs[!(SS_bootstrap_freqs$Var1=="Black" & SS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    SS_bootstrap_freqs=total
  }
  #for whitehispanic
  validation = subset.data.frame(SS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(SS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
    total = SS_bootstrap_freqs[!(SS_bootstrap_freqs$Var1=="Hispanic" & SS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq + add$Freq
    SS_bootstrap_freqs=total
  }
  #for asianhispanic
  validation = subset.data.frame(SS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(SS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
    total = SS_bootstrap_freqs[!(SS_bootstrap_freqs$Var1=="Hispanic" & SS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq + add$Freq
    SS_bootstrap_freqs=total
  }
  
  
  y <- rbind(y, SS_bootstrap_freqs, stop)
  
}
colnames(y) <- c("off1", "off2", "freq")
SeattleBootstrap <- y

View(SeattleBootstrap)

#making individual frequency datasets for all of the race combinations
#whitewhite
whitewhite <- subset(SeattleBootstrap, SeattleBootstrap$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")
whitewhite$freq <- as.numeric(whitewhite$freq)

#whiteblack
whiteblack <- subset(SeattleBootstrap, SeattleBootstrap$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
whiteblack$freq <- as.numeric(whiteblack$freq)

#blackblack
blackblack <- subset(SeattleBootstrap, SeattleBootstrap$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")
blackblack$freq <- as.numeric(blackblack$freq)

#hispanichispanic
hispanichispanic <- subset(SeattleBootstrap, SeattleBootstrap$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")
hispanichispanic$freq <- as.numeric(hispanichispanic$freq)

#hispanicwhite 
hispanicwhite <- subset(SeattleBootstrap, SeattleBootstrap$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "Hispanic")
hispanicwhite$freq <- as.numeric(hispanicwhite$freq)

#hispanicasian
hispanicasian <- subset(SeattleBootstrap, SeattleBootstrap$off1 == "Asian")
hispanicasian <- subset(hispanicasian, hispanicasian$off2 == "Hispanic")
hispanicasian$freq <- as.numeric(hispanicasian$freq)


median(whitewhite$freq) #actual = 8, bootstrap = 8
median(whiteblack$freq) #actual = 2, bootstrap = 5
median(hispanicwhite$freq) #actual = 1, bootstrap = 0
median(asianwhite$freq) #actual = 2, bootstrap = 0

hist(asianwhite$freq)
