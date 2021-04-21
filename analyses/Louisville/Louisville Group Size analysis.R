#libraries
library(here)
library(ggplot2)
library(dplyr)

#calling datasets
LouisvilleShootings<-read.csv(file = here('clean data/Louisville/LouisvilleShootings.csv'), stringsAsFactors = FALSE)
AustinShootings<- read.csv(file = here('clean data/Austin/Shootings_Austin.csv'))
OrlandoShootings<- read.csv(file = here('clean data/Orlando/shooting (cleaned).csv'))
OrlandoUOF<- read.csv(file = here('clean data/Orlando/UOF (cleaned).csv'))

#the function
OfficerGroupSize <- function(dataset, mergecol, together){
  matching <- table(together)
  matching <- as.data.frame(matching)
  colnames(matching) <- c(mergecol, "officer_group_size")
  dataset <- merge(matching, dataset, by = mergecol, all.y = TRUE)
  return(dataset)
}

#running the function on all datasets that need it
LouisvilleShootings <- OfficerGroupSize(LouisvilleShootings, "PIU_number", LouisvilleShootings$PIU_number)
AustinShootings<- OfficerGroupSize(AustinShootings, "Case", AustinShootings$Case)

#taking out duplicates of case numbers
LouisvilleShootings<- LouisvilleShootings[!duplicated(LouisvilleShootings$PIU_number),]
AustinShootings<- AustinShootings[!duplicated(AustinShootings$Case),]

View(table(LouisvilleShootings$officer_group_size))
View(table(AustinShootings$officer_group_size))
View(table(OrlandoShootings$Number.Of.Officers.Involved))
View(table(OrlandoUOF$Officers.Involved))

#graphs
ggplot(LouisvilleShootings, aes(officer_group_size)) +
  geom_bar()

ggplot(AustinShootings, aes(officer_group_size)) +
  geom_bar()

ggplot(OrlandoShootings, aes(Number.Of.Officers.Involved)) +
  geom_bar()

ggplot(OrlandoUOF, aes(Officers.Involved)) +
  geom_bar() +
  scale_x_continuous("Officers.Involved", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
  

