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

View(DS_race)
DS_race<-subset(DallasR2R, select = c("officer_race","officer_name" ))
DS_race <- unique(DS_race)
DS_race<-subset(DallasR2R, select = "officer_race")
DS_race[DS_race == "Other"] <- NA
DS_race <- na.omit(DS_race)

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
  DS_bootstrap_freqs <- add_zero_row("White", "Black", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Black", "White", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Hispanic", "White", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("White", "Hispanic", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Black", "Hispanic", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Hispanic", "Black", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("White", "Asian", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Asian", "White", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Black", "Black", DS_bootstrap_freqs)
  DS_bootstrap_freqs <- add_zero_row("Asian", "Hispanic", DS_bootstrap_freqs)
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
  
  #for blackhispanic
  validation = subset.data.frame(DS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Black')
  if (nrow(validation) != 0){
    add = subset.data.frame(DS_bootstrap_freqs,Var1=='Hispanic' & Var2=='Black')
    total = DS_bootstrap_freqs[!(DS_bootstrap_freqs$Var1=="Hispanic" & DS_bootstrap_freqs$Var2=='Black'),]
    total[(total$Var1=='Black' & total$Var2=='Hispanic'),]$Freq = total[(total$Var1=='Black' & total$Var2=='Hispanic'),]$Freq + add$Freq
    DS_bootstrap_freqs=total
  }
  
  #for asianwhite
  validation = subset.data.frame(DS_bootstrap_freqs,Var1=='White' & Var2=='Asian')
  if (nrow(validation) != 0){
    add = subset.data.frame(DS_bootstrap_freqs,Var1=='White' & Var2=='Asian')
    total = DS_bootstrap_freqs[!(DS_bootstrap_freqs$Var1=="White" & DS_bootstrap_freqs$Var2=='Asian'),]
    total[(total$Var1=='Asian' & total$Var2=='White'),]$Freq = total[(total$Var1=='Asian' & total$Var2=='White'),]$Freq + add$Freq
    DS_bootstrap_freqs=total
  }
  
  y <- rbind(y, DS_bootstrap_freqs, stop)
 
}
colnames(y) <- c("off1", "off2", "freq")

DallasBootstrapUnique <- y 
DallasBootstrap <- y 

#With Unique data 
whitewhite <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")
whitewhite$freq <- as.numeric(whitewhite$freq)

#whiteblack
whiteblack <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
whiteblack$freq <- as.numeric(whiteblack$freq)

#blackblack
blackblack <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")
blackblack$freq <- as.numeric(blackblack$freq)

#hispanichispanic
hispanichispanic <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")
hispanichispanic$freq <- as.numeric(hispanichispanic$freq)

#hispanicwhite 
hispanicwhite <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "Hispanic")
hispanicwhite$freq <- as.numeric(hispanicwhite$freq)

#hispanicasian
hispanicasian <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "Asian")
hispanicasian <- subset(hispanicasian, hispanicasian$off2 == "Hispanic")
hispanicasian$freq <- as.numeric(hispanicasian$freq)

#blackhispanic
blackhispanic <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "Black")
blackhispanic <- subset(blackhispanic, blackhispanic$off2 == "Hispanic")
blackhispanic$freq <- as.numeric(blackhispanic$freq)

#asianwhite
asianwhite <- subset(DallasBootstrapUnique, DallasBootstrapUnique$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite$freq <- as.numeric(asianwhite$freq)






#making individual frequency datasets for all of the race combinations
#whitewhite
whitewhite <- subset(DallasBootstrap, DallasBootstrap$off1 == "White")
whitewhite <- subset(whitewhite, whitewhite$off2 == "White")
whitewhite$freq <- as.numeric(whitewhite$freq)

#whiteblack
whiteblack <- subset(DallasBootstrap, DallasBootstrap$off1 == "White")
whiteblack <- subset(whiteblack, whiteblack$off2 == "Black")
whiteblack$freq <- as.numeric(whiteblack$freq)

#blackblack
blackblack <- subset(DallasBootstrap, DallasBootstrap$off1 == "Black")
blackblack <- subset(blackblack, blackblack$off2 == "Black")
blackblack$freq <- as.numeric(blackblack$freq)

#hispanichispanic
hispanichispanic <- subset(DallasBootstrap, DallasBootstrap$off1 == "Hispanic")
hispanichispanic <- subset(hispanichispanic, hispanichispanic$off2 == "Hispanic")
hispanichispanic$freq <- as.numeric(hispanichispanic$freq)

#hispanicwhite 
hispanicwhite <- subset(DallasBootstrap, DallasBootstrap$off1 == "White")
hispanicwhite <- subset(hispanicwhite, hispanicwhite$off2 == "Hispanic")
hispanicwhite$freq <- as.numeric(hispanicwhite$freq)

#hispanicasian
hispanicasian <- subset(DallasBootstrap, DallasBootstrap$off1 == "Asian")
hispanicasian <- subset(hispanicasian, hispanicasian$off2 == "Hispanic")
hispanicasian$freq <- as.numeric(hispanicasian$freq)

#blackhispanic
blackhispanic <- subset(DallasBootstrap, DallasBootstrap$off1 == "Black")
blackhispanic <- subset(blackhispanic, blackhispanic$off2 == "Hispanic")
blackhispanic$freq <- as.numeric(blackhispanic$freq)

#asianwhite
asianwhite <- subset(DallasBootstrap, DallasBootstrap$off1 == "Asian")
asianwhite <- subset(asianwhite, asianwhite$off2 == "White")
asianwhite$freq <- as.numeric(asianwhite$freq)


View(whitewhite)
View(blackblack)
View(hispanichispanic)

hist(whitewhite$freq)
hist(blackblack$freq)
hist(hispanichispanic$freq)
hist(whiteblack$freq)


#This is 20 whitewhite, 11 whitelatinx, 5 whiteblack, 3 blackblack, 2 latinxlatinx, and 1 latinxasian

median(whitewhite$freq) #actual = 20, bootstrap = 15, bootstrap unique = 15
median(blackblack$freq) #actual = 3, bootstrap = 1, bootstrap unique = 1
median(whiteblack$freq) #actual = 5, bootstrap = 7, bootstrap unique = 7
median(hispanicwhite$freq) #actual = 11, bootstrap = 15, bootstrap unique = 11
median(hispanichispanic$freq) #actual = 2, bootstrap = 2, bootstrap unique = 2
median(hispanicasian$freq) #actual = 1, bootstrap = 0, bootstrap unique = 0
median(blackhispanic$freq) #actual = 0, bootstrap = 1, bootstrap unique = 1
median(asianwhite$freq) #actual = 0, bootstrap = 1, bootstrap unique = 1

#Using the unique list does not make much of a difference, other than changing hispanicwhite. I'm pretty confused by this and would love to discuss. 

#From this we know that groups of whitewhite people are move involved in shootings then they would be if it was random
#according to my AP stats level calculations, assuming that this can be considered an approximately normal sampling distribution, 
#the is a 7.5% we could have gotten 20 or more whitewhite officers randomly. 

#for calculations
hist(whitewhite$freq)
mean(whitewhite$freq)
sd(whitewhite$freq)

#Racially diverse vs. homologous 
homologousgroups <- NULL
homologousgroups <- whitewhite$freq + hispanichispanic$freq + blackblack$freq
homologousgroups <- as.data.frame(homologousgroups)
colnames(homologousgroups) <- "freq"
median(homologousgroups$freq)
View(homologousgroups)

diversegroups <- NULL
diversegroups <- hispanicwhite$freq + whiteblack$freq + hispanicasian$freq + blackhispanic$freq + asianwhite$freq
diversegroups <- as.data.frame(diversegroups)
colnames(diversegroups) <- "freq"
median(diversegroups$freq)

#Actual amount: diverse:17 homologous:25
#Bootstrap amounts: diverse:23 homologous:19
#The bootstrap adds up to 42 instead of 43 but I think it is just rounding issues.
# Do this for other cities 

hist(homologousgroups$freq)
hist(diversegroups$freq)

#both of these are pretty unlikely, but it would take me more time to figure out if this is statistically significant or not. 



