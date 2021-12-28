#Replicating bootstrapping 
install.packages("here")
library(here)
install.packages("data.table")
library(data.table)
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ggplot2)

#For Austin
AustinShootings <- read.csv(here("clean data", "Austin", "Shootings_Austin.csv"), stringsAsFactors = FALSE)
AustinUOF <- read.csv(here("dirty data", "Austin", "R2R_2018.csv"), stringsAsFactors = FALSE)
View(AustinUOF)

OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

AustinShootings <- OfficerGroupSize(AustinShootings, "Case", AustinShootings$Case)


#The bootstrapping 
AS_race<-subset(AustinShootings, select = "Officer.Race") #This should not be subsetting from AustinShootings, preferably a dataset more representative of the general police department.
#After much reserach, I am unable to find another dataset that has officer race data for austin anywhere. Is this worth doing a FOIA request? If we get a dataset, i would simply need to plug it in here and the code should run well. 

y  <- NULL;
for (i in 1:1000) {
  AS_bootstrap <- replicate(2, AS_race[sample(nrow(AS_race), 11, replace = TRUE), ])
  colnames(AS_bootstrap) <- c("off1", "off2")
  AS_bootstrap <- as.data.frame(AS_bootstrap)
  AS_bootstrap_freqs <- data.frame(table(AS_bootstrap$off1, AS_bootstrap$off2))
  validation = subset.data.frame(AS_bootstrap_freqs,Var1=='Black' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(AS_bootstrap_freqs,Var1=='Black' & Var2=='White')
    total = AS_bootstrap_freqs[!(AS_bootstrap_freqs$Var1=="Black" & AS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Black'),]$Freq = total[(total$Var1=='White' & total$Var2=='Black'),]$Freq + add$Freq
    AS_bootstrap_freqs=total
  }
  
  #for hispanic
  validation = subset.data.frame(AS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
  if (nrow(validation) != 0){
    add = subset.data.frame(AS_bootstrap_freqs,Var1=='Hispanic' & Var2=='White')
    total = AS_bootstrap_freqs[!(AS_bootstrap_freqs$Var1=="Hispanic" & AS_bootstrap_freqs$Var2=='White'),]
    total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='White' & total$Var2=='Hispanic'),]$Freq + add$Freq
    AS_bootstrap_freqs=total
  }
  
  y <- rbind(y, AS_bootstrap_freqs)
}
colnames(y) <- c("off1", "off2", "freq")
View(y)

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

#hispanichispanic
hispanichispanic <- subset(y, y$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")

#hispanicwhite 
hispanicwhite <- subset(y, y$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "White")

#Visualization
View(whitewhite)
hist(whitewhite$freq)
hist(blackblack$freq)
hist(whiteblack$freq)
hist(hispanichispanic$freq)


#Calculating the actual frequencies for comparision
AS_race2 <- subset(AustinShootings, AustinShootings$officer_group_size == 2)
x<-subset(AS_race2, select = c("Case", "Officer.Race"))
x<- x %>%
  group_by(grp = str_c('Column', rep(1:2, length.out = n()))) %>%
  mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = Officer.Race) %>%
  select(-rn)

AS_race2 <- x
View(AS_race2)
AS_race2[ ,"Case"] <- list(NULL)
colnames(AS_race2) <- c("off1", "off2")
AS_group_freqs <- data.frame(table(AS_race2$off1, AS_race2$off2))

View(AS_group_freqs) #this is the actual data for the groups of 2 officers that ended up shooting people
#It is 6 whitewhite, 3 hispanicwhite, 1 blackwhite, and 1 hispanichispanic


#We can use the bootstrapping to figure out if this is random or not. 
median(whitewhite$freq) #actual = 6
median(blackblack$freq) #actual = 0
median(blackwhite$freq) #actual = 1
median(hispanicwhite$freq) #actual = 3
median(hispanichispanic$freq) #actual = 1

#To do -  find different dataset to take bootstrapping from, find statistical analysis to measure significance. 



