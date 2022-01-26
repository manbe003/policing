library(here)
library(epitools)
library(tidyverse)


UOF <- read.csv(file=here('clean data/New Orleans/New Orleans UOF.csv'), stringsAsFactors = FALSE)

UOF_All_FixLevels <- UOF
UOF_All_FixLevels <- UOF_All_FixLevels %>%
  mutate(Force.Type = case_when(
    str_detect(Force.Type, "Discharged") ~ "3",
    str_detect(Force.Type, "Vehicle as Weapon") ~ "3",
    str_detect(Force.Type, "Escort Tech") ~ "2",
    str_detect(Force.Type, "CEW") ~ "2",
    str_detect(Force.Type, "Canine") ~ "2",
    str_detect(Force.Type, "Baton") ~ "2",
    str_detect(Force.Type, "NonTrad Impact Weapon") ~ "2",
    str_detect(Force.Type, "Pointed") ~ "1",
    str_detect(Force.Type, "Exhibited") ~ "1",
    str_detect(Force.Type, "Canine (No Bite)") ~ "1",
    str_detect(Force.Type, "Hands") ~ "1",
    str_detect(Force.Type, "Take Down") ~ "1",
    str_detect(Force.Type, "Takedown") ~ "1",
    str_detect(Force.Type, "Head Strike") ~ "1",
    str_detect(Force.Type, "Force") ~ "1",
    str_detect(Force.Type, "Handcuffed Subject") ~ "1",
    TRUE ~ Force.Type
  ))


probability<-function(df, colnum){
  x<-cbind(table(df[,colnum]))
  
  print("odds of escalating to force of 3")
  print(x[3,1]/sum(x[,1]))
  
  print("odds of escalating past force of 1")
  print(sum(x[2:3,1])/sum(x[,1]))
}

probability(UOF_All_FixLevels, 5)

UOF_FixLevels<-split(UOF_All_FixLevels, f=(UOF_All_FixLevels$Binning.Number.of.Officers))
probability(UOF_FixLevels$`1`,5)
probability(UOF_FixLevels$`2`,5)
probability(UOF_FixLevels$`3`,5)


#now looking to do a chi-square test to assess difference
#first make a table of number of officers by force type
dt <- table(UOF_All_FixLevels$Binning.Number.of.Officers,UOF_All_FixLevels$Force.Type)

dt

#Graph
balloonplot(t(dt), main ="Force.Type", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dt, shade = TRUE, las=2,main = "PD.Force.Type")

#could do chi square of 1 vs 2/3 and 1 vs 3 but only for 1 vs 3+ officers
dt<-dt[c(1,3),]
mosaicplot(dt, shade = TRUE, las=2,main = "PD.Force.Type")
chi<-chisq.test(dt)
chi
