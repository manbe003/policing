#libraries
library(ggplot2)
library(here)
library(dplyr)
library(tidyr)

#WD
DemoData_UOF<-read.csv(file=here('clean data/orlando/Orlando City DemoData UOF.csv'), stringsAsFactors = FALSE)
DemoData_Shootings<-read.csv(file=here('clean data/orlando/Orlando City DemoData Shootings.csv'), stringsAsFactors = FALSE)
Shootings<-read.csv(file=here('clean data/orlando/shooting (cleaned).csv'), stringsAsFactors = FALSE)


Demodata = data.frame(count=c(192770,61670,957,9084,50,13574), category=c("white","black", "Native America","Asian","Native Hawaiian","Mixed"))

  
Demodata$fraction = Demodata$count / sum(Demodata$count)
Demodata = Demodata[order(Demodata$fraction), ]
Demodata$ymax = cumsum(Demodata$fraction)
Demodata$ymin = c(0, head(Demodata$ymax, n=-1))


ggplot(Demodata, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="Basic ring plot")


#separating rows so there is only one offender per row
Shootings_Offenders<- Shootings %>% separate_rows(Suspect.Race, Suspect.Gender, Suspect.Hit, Fatal, sep= ",")


ggplot(data = as.data.frame(Shootings_Offenders$Suspect.Race), 
       aes(x = factor(1),fill = factor(Shootings_Offenders$Suspect.Race))) + 
  geom_bar(stat = "count") +
 scale_y_continuous(breaks = seq(0,length(Shootings_Offenders$Suspect.Race),length(Shootings_Offenders$Suspect.Race)/4), labels = c("0", "25%", "50%", "75%", "100%")) + 
  coord_polar(theta='y') +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(fill = "Suspect.Race")


