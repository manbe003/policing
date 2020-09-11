#EDA for Seattle
library(tidyverse)
#summary data
setwd("C:/Users/dmcmanu1/policing/clean data/seattle")
UOF<-read.csv(file='shootings_Seattle.csv', stringsAsFactors = FALSE)


#Descriptive Data
summary(UOF)
var(UOF$Longitude)
sd(UOF$Longitude)
#Age of subject is not normal has a peak around 20 and 30








hist(UOF$Subject.Age, breaks=10)
UOF$Subject.Age==114

boxplot(UOF$Subject.Age) 


ggplot(UOF, aes(sample = Subject.Age)) + 
  geom_qq() +
  geom_qq_line()
