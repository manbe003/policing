#libraries
library(tidyr)
library(dplyr)
library(here)


##Categrical data because there is no descriptive data
#call files
Shootings<-read.csv(file=here('clean data/orlando/Shooting (cleaned).csv'), stringsAsFactors = TRUE)
UOF<-read.csv(file=here('clean data/orlando/UOF (cleaned).csv'), stringsAsFactors = FALSE)

#deleting rows where there are 6 races listed, but only 5 ethnicities listed.
UOF<- UOF[-c(3041, 4376, 5295, 1125), ]

UOF<- UOF %>% separate_rows(Offenders.Race, Offenders.Sex, Offenders.Ethnicity, sep= ";")




summary(UOF)
summary(Shootings)

#levels
levels(UOF$Officers.Race)
levels(UOF$Officers.Ethnicity)
levels(UOF$Officers.Sex)
levels(UOF$Offenders.Race)
levels(UOF$Offenders.Ethnicity)
levels(UOF$Offenders.Ethnicity)
levels(UOF$Offenders.Sex)

#graphs
ggplot(UOF, aes(Officers.Race)) +
  geom_bar()
