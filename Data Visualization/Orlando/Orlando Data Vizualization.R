#load dependencies and set working directory
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()
setwd(here())

#loading files
DemoData_UOF<-read.csv(file=here('clean data/orlando/Orlando City DemoData UOF.csv'), stringsAsFactors = FALSE)
DemoData_Shootings<-read.csv(file=here('clean data/orlando/Orlando City DemoData Shootings.csv'), stringsAsFactors = FALSE)
Shootings<-read.csv(file=here('clean data/orlando/shooting (cleaned).csv'), stringsAsFactors = FALSE)

#demodata pie chart as practice
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

##Shootings data doughnut chart practice
#separating rows so there is only one offender per row
Shootings_Offenders<- Shootings %>% separate_rows(Suspect.Race, Suspect.Gender, Suspect.Hit, Fatal, sep= ",")


ggplot(data = as.data.frame(na.omit(Shootings_Offenders$Suspect.Race, na.rm = TRUE))) + 
       aes(x = factor(1),fill = factor(na.omit(Shootings_Offenders$Suspect.Race))) + 
  geom_bar(stat = "count") +
 scale_y_continuous(breaks = seq(0,length(na.omit(Shootings_Offenders$Suspect.Race)),length(na.omit(Shootings_Offenders$Suspect.Race))/4), labels = c("0", "25%", "50%", "75%", "100%")) + 
  coord_polar(theta='y') +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(fill = "Suspect.Race")

#making a demo doughnut plot for just the races the shootings dataset includes
Demodata_shoot = data.frame(count=c(61670,23665,192770), category=c("Black","Other", "White"))


Demodata_shoot$fraction = Demodata_shoot$count / sum(Demodata_shoot$count)
Demodata_shoot = Demodata_shoot[order(Demodata_shoot$fraction), ]
Demodata_shoot$ymax = cumsum(Demodata_shoot$fraction)
Demodata_shoot$ymin = c(0, head(Demodata_shoot$ymax, n=-1))

ggplot(Demodata_shoot, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="Shootings Demo Data")





##trying to combine the charts/make a double doughnut chart
#making a combined data set
shoot = as.data.frame(table(na.omit(Shootings_Offenders$Suspect.Race)))
demo = data.frame(category=c("Black","Other", "White"), count=c(61670,23665,192770))

combined <- merge(shoot, demo, by.x= "Var1", by.y= "category" )
colnames(combined)[1] <- "Race"
colnames(combined)[2] <- "police_freq"
colnames(combined)[3] <- "demo_freq"

# doughnut chart w demo data/ proof of concept
combined$fraction = combined$demo_freq / sum(combined$demo_freq)
combined = combined[order(combined$fraction), ]
combined$ymax = cumsum(combined$fraction)
combined$ymin = c(0, head(combined$ymax, n=-1))

ggplot(combined, aes(fill=Race, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="Basic ring plot")


## doughnut chart w shoot data/ proof of concept
combined$fraction2 = combined$police_freq / sum(combined$police_freq)
combined = combined[order(combined$fraction2), ]
combined$ymax2 = cumsum(combined$fraction2)
combined$ymin2 = c(0, head(combined$ymax2, n=-1))

ggplot(combined, aes(fill=Race, ymax=ymax2, ymin=ymin2, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="Basic ring plot 2")

  
  
###combined chart
#different combined, frequencies stolen from the combined table, calculated above 
df <- data.frame (
  type = rep(c("demo", "police"), each = 3), 
   Race = rep(c("Black", "White", "Other"), 2),
  freq = c(0.22175078, 0.69315546, 0.08509376, 0.69117647, 0.29411765, 0.01470588)
)

ggplot(df, aes(x = type, y = freq, fill = Race)) +
  geom_col()+
  scale_x_discrete(limits = c(" ", "police","demo")) +
  scale_fill_viridis_d() +
  coord_polar("y")






