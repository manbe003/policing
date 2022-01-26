#Dallas Bootstrapping 

#Replicating bootstrapping 
install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ggplot2)

DallasShootings <- read.csv(here("clean data", "Dallas", "Dallas_shootings.csv"), stringsAsFactors = FALSE)
DallasR2R <- read.csv(here("clean data", "Dallas", "Dallas_R2R.csv"), stringsAsFactors = FALSE)

View(DallasR2R)

OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

DallasShootings <- OfficerGroupSize(DallasShootings, "subject_name", DallasShootings$subject_name)

#Calculating the actual frequencies for comparision
DS_race2 <- subset(DallasShootings, DallasShootings$officer_group_size == 2)
x<-subset(DS_race2, select = c("subject_name", "officer_race"))
x<- x %>%
  group_by(grp = str_c('Column', rep(1:2, length.out = n()))) %>%
  mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = officer_race) %>%
  select(-rn)

DS_race2 <- x
DS_race2[ ,"subject_name"] <- list(NULL)
colnames(DS_race2) <- c("off1", "off2")
DS_group_freqs <- data.frame(table(DS_race2$off1, DS_race2$off2))

View(DS_group_freqs)
#This is 20 whitewhite, 11 whitelatinx, 5 whiteblack, 3 blackblack, 2 latinxlatinx, and 1 latinxasian


#The bootstrapping 

DS_race<-subset(DallasR2R, select = "officer_race")
Var1 <- "stop"
Var2 <- "stop"
Freq <- "stop"
stop <- data.frame(Var1, Var2, Freq)

y  <- NULL;
for (i in 1:1000) {
  DS_bootstrap <- replicate(2, DS_race[sample(nrow(DS_race), 43, replace = TRUE), ])
  colnames(DS_bootstrap) <- c("off1", "off2")
  DS_bootstrap <- as.data.frame(DS_bootstrap)
  DS_bootstrap_freqs <- data.frame(table(DS_bootstrap$off1, DS_bootstrap$off2))
  validation = subset.data.frame(DS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(DS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = DS_bootstrap_freqs[!(DS_bootstrap_freqs$Var1=="Black" & DS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    DS_bootstrap_freqs=total
  }
  
  #for hispanicwhite
  validation = subset.data.frame(DS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(DS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
    total = DS_bootstrap_freqs[!(DS_bootstrap_freqs$Var1=="Hispanic" & DS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq + add$Freq
    DS_bootstrap_freqs=total
  }
  
  #for hispanicasian
  validation = subset.data.frame(DS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(DS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Asian')
    total = DS_bootstrap_freqs[!(DS_bootstrap_freqs$Var1=="Hispanic" & DS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='Hispanic'),]$Freq + add$Freq
    DS_bootstrap_freqs=total
  }
  
  y <- rbind(y, DS_bootstrap_freqs, stop)
 
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



View(whitewhite)
hist(whitewhite$freq)
hist(blackblack$freq)
hist(whiteblack$freq)


#This is 20 whitewhite, 11 whitelatinx, 5 whiteblack, 3 blackblack, 2 latinxlatinx, and 1 latinxasian

median(whitewhite$freq) #actual = 20, bootstrap = 15
median(blackblack$freq) #actual = 3, bootstrap = 1
median(blackwhite$freq) #actual = 5, bootstrap = 2
median(hispanicwhite$freq) #actual = 11, bootstrap = 15
median(hispanichispanic$freq) #actual = 2, bootstrap = 2
median(hispanicasian$freq) #actual = 1, bootstrap = 0

#Does not add to 43 because we are not counting the categories that are 0 in real life
#for instance, whiteasian, hispanicblack, and anything with indigenous or other all show up in our bootstrapping but not in the actual data.
#I can maybe delete other but idk what to do about the rest. 



