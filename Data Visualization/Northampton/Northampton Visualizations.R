#libraries
library(here)
library(tidyverse)

#loading in datasets
UOF<- read.csv(file=here('clean data/Northampton/Northampton UOF.csv'), stringsAsFactors = FALSE)

ggplot(UOF,
       aes(x = Call.Type,
           fill = PD.Force.Type))+
  geom_bar(position = "dodge")

ggplot(UOF,
       aes(x = Subject.Race,
           fill = PD.Force.Type))+
  geom_bar(position = "dodge")






