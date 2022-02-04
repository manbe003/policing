
#Just some testing 
testing <- read.csv(here("clean data", "Seattle", "shootings_Seattle.csv"), stringsAsFactors = FALSE)
testing <- read.csv(here("clean data", "Dallas", "Dallas_R2R.csv"), stringsAsFactors = FALSE)
testing <- read.csv(here("clean data", "Louisville", "LouisvilleStops.csv"), stringsAsFactors = FALSE)
testing <- read.csv(here("clean data", "Norwich", "norwich_UOF.csv"), stringsAsFactors = FALSE)
testing <- read.csv(here("clean data", "Northampton", "Northampton UOF.csv"), stringsAsFactors = FALSE)
testing <- read.csv(here("clean data", "New Orleans", "New Orleans UOF.csv"), stringsAsFactors = FALSE)
testing <- read.csv(here("clean data", "New Orleans", "New Orleans UOF.csv"), stringsAsFactors = FALSE)


View()

#Orlando Bootstrapping 

install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ggplot2)

OrlandoShootings <- read.csv(here("clean data", "Orlando", "shooting (cleaned).csv"), stringsAsFactors = FALSE)
OrlandoR2R <- read.csv(here("clean data", "Orlando", "UOF (cleaned).csv"), stringsAsFactors = FALSE)

View(OrlandoR2R)
View(OrlandoShootings)


#Running into some problems. Says white for race but hispanic for ethnicity sometimes - idk how to replace without messing everything uo but seems bad to just ignore hispanic people 
#Especially because white white might look not diverse but actually it is white hispanic so it is? I'm not sure. 
#I'm just going to move on from Orland for now and then we can decide if I should go back and try again. 




#Calculating the actual frequencies for comparision
OS_race2 <- subset(OrlandoShootings, OrlandoShootings$Number.Of.Officers.Involved == 2)
View(OS_race2)
#This is 12 total, 7 whitewhite, 


#The bootstrapping 
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

OrlandoR2R = separate_rows(OrlandoR2R,"Officers.Race",sep = ";")
OS_race<-subset(OrlandoR2R, select = "Officers.Race")
OS_race <- separate_rows(OS_race,"Officers.Race",sep = ";")
OS_race[OS_race == "Other"] <- NA
OS_race <- na.omit(OS_race)


Var1 <- "stop"
Var2 <- "stop"
Freq <- "stop"
stop <- data.frame(Var1, Var2, Freq)

y  <- NULL;
for (i in 1:1000) {
  OS_bootstrap <- replicate(2, OS_race[sample(nrow(OS_race), 12, replace = TRUE), ])
  colnames(OS_bootstrap) <- c("off1", "off2")
  OS_bootstrap <- as.data.frame(OS_bootstrap)
  OS_bootstrap_freqs <- data.frame(table(OS_bootstrap$off1, OS_bootstrap$off2))
  OS_bootstrap_freqs <- add_zero_row("White", "Black", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("Black", "White", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("Hispanic", "White", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("White", "Hispanic", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("Black", "Hispanic", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("Hispanic", "Black", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("White", "Asian", OS_bootstrap_freqs)
  OS_bootstrap_freqs <- add_zero_row("Asian", "White", OS_bootstrap_freqs)
  validation = subset.data.frame(OS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(OS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = OS_bootstrap_freqs[!(OS_bootstrap_freqs$Var1=="Black" & OS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    OS_bootstrap_freqs=total
  }
  
  #for hispanicwhite
  validation = subset.data.frame(OS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(OS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
    total = OS_bootstrap_freqs[!(OS_bootstrap_freqs$Var1=="Hispanic" & OS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq + add$Freq
    OS_bootstrap_freqs=total
  }
  
  #for hispanicasian
  validation = subset.data.frame(OS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(OS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
    total = OS_bootstrap_freqs[!(OS_bootstrap_freqs$Var1=="Hispanic" & OS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq + add$Freq
    OS_bootstrap_freqs=total
  }
  
  #for blackhispanic
  validation = subset.data.frame(OS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Black')
  if (nrow(validation) != 0){
    add = subset.data.frame(OS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Black')
    total = OS_bootstrap_freqs[!(OS_bootstrap_freqs$Var1=="Hispanic" & OS_bootstrap_freqs$Var2=='Black'),]
    total[(total$Var1=='Black' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Black' & total$Var2=='Hispanic'),]$Freq + add$Freq
    OS_bootstrap_freqs=total
  }
  
  #for asianwhite
  validation = subset.data.frame(OS_bootstrap_freqs,Var1=='White' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(OS_bootstrap_freqs,Var1=='White' & Var2=='Asian')
    total = OS_bootstrap_freqs[!(OS_bootstrap_freqs$Var1=="White" & OS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='White'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='White'),]$Freq + add$Freq
    OS_bootstrap_freqs=total
  }
  
  y <- rbind(y, OS_bootstrap_freqs, stop)
  
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
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "White")
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


median(whitewhite$freq) #actual = , bootstrap = 
median(blackblack$freq) #actual = , bootstrap = 
median(whiteblack$freq) #actual = , bootstrap = 
median(hispanicwhite$freq) #actual = , bootstrap = 
median(hispanichispanic$freq) #actual = , bootstrap = 
median(hispanicasian$freq) #actual = , bootstrap = 
median(blackhispanic$freq) #actual = , bootstrap = 
median(asianwhite$freq) #actual = , bootstrap = 
