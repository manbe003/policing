#Seattle EDA
library(tidyverse)

setwd("C:/Users/dmcmanu1/policing/clean data/seattle")


##Descriptive

shootings = read.csv(file = "shootings_Seattle.csv", stringsAsFactors = FALSE)
summary(shootings)


shootings$Years.of.SPD.Service = as.numeric(shootings$Years.of.SPD.Service)
summary(x)
sd(x)
test = na.exclude(shootings$Years.of.SPD.Service)
sd(y)



##Visual

hist(y)
hist(y, breaks= 100)
hist(shootings$Subject.Age)

##Outliers

boxplot(shootings$Subject.Age)

##Normality Test
ggplot(shootings, aes(sample = Years.of.SPD.Service)) + 
  geom_qq() +
  geom_qq_line()
