#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)
library(chron)

#set working directory
setwd("~/policing/policing/clean data/seattle")

#I want to call in my datasets.
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = TRUE)
shootings<-read.csv(file='shootings_Seattle.csv', stringsAsFactors = TRUE)
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = TRUE)

UOF_level3<-subset(UOF, UOF$Incident_Type == "Level 3 - OIS" | UOF$Incident_Type == "Level 3 - Use of Force")

#make a UOF dataset with only distinct incidents
UOF_level3<-distinct(UOF_level3,Incident_Num,.keep_all=TRUE)
SplitTime_UOF<-strsplit(as.character(UOF_level3$time),":")
SplitTime_UOF<-do.call(rbind, SplitTime_UOF)
SplitTime_UOF<-as.data.frame(SplitTime_UOF, stringsAsFactors=FALSE)
colnames(SplitTime_UOF)<-(c("hour","minute"))
UOF_level3$splittime<-SplitTime_UOF$hour
UOF_level3$Date<-c(as.Date(UOF_level3$Date, "%m/%d/%Y"))

UOF_level3<-distinct(UOF_level3, Date, splittime, .keep_all = TRUE)
#spotclean
UOF_level3<-rbind(UOF_level3[1:26,], UOF_level3[28:30,], UOF_level3[32:33,], UOF_level3[35:39,], UOF_level3[42:43,], UOF_level3[45:49,], UOF_level3[51:70,], UOF_level3[73:104,])


#make a shootings without identical times
shootings_distinct<-distinct(shootings, Date, time, .keep_all=TRUE)

#make a shootings without times in same hours
shootings_hour<-strsplit(as.character(shootings_distinct$time),":")
shootings_hour<-do.call(rbind, shootings_hour)
shootings_hour<-as.data.frame(shootings_hour, stringsAsFactors=FALSE)
colnames(shootings_hour)<-(c("hour","minute","second"))

shootings_distinct$hour<-as.numeric(shootings_hour$hour)
shootings_distinct$minute<-(as.numeric(shootings_hour$minute)/60)
shootings_distinct<-distinct(shootings_distinct, Date, hour, .keep_all=TRUE)

#make a shootings without rounded times
shootings_distinct$rounded<-shootings_distinct$hour+shootings_distinct$minute
shootings_distinct$rounded<-round(shootings_distinct$rounded)
shootings_distinct<-distinct(shootings_distinct, Date, rounded, .keep_all=TRUE)

#remove others by eye
shootings_distinct<-rbind(shootings_distinct[1:3,], shootings_distinct[5:13,], shootings_distinct[15:20,], shootings_distinct[22,], shootings_distinct[25:63,], shootings_distinct[65,],shootings_distinct[67:76,],shootings_distinct[78:84,])
#make date a date variable
shootings_distinct$Date<-c(as.Date(shootings_distinct$Date, "%m/%d/%Y"))


#here I want to see what OIS incidents are represented in both datasets
overlap_UOF_shootings<-inner_join(UOF_level3, shootings_distinct, by="Date")

#okay so only a little overlap. Now I need to create a joint dataset. First I'll count how many per year to compare with https://patch.com/washington/seattle/36-people-shot-seattle-police-2010-report

Splitdate_shooting<-strsplit(as.character(shootings_distinct$Date),"-")
Splitdate_shooting<-do.call(rbind, Splitdate_shooting)
Splitdate_shooting<-as.data.frame(Splitdate_shooting, stringsAsFactors=FALSE)
colnames(Splitdate_shooting)<-(c("year","month","day"))
table(Splitdate_shooting$year)

UOF_OIS<-subset(UOF_level3, UOF_level3$Incident_Type == "Level 3 - OIS")

Splitdate_UOF<-strsplit(as.character(UOF_OIS$Date),"-")
Splitdate_UOF<-do.call(rbind, Splitdate_UOF)
Splitdate_UOF<-as.data.frame(Splitdate_UOF, stringsAsFactors=FALSE)
colnames(Splitdate_UOF)<-(c("year","month", "day"))
table(Splitdate_UOF$year)

Splitdate_overlap<-strsplit(as.character(overlap_UOF_shootings$Date),"-")
Splitdate_overlap<-do.call(rbind, Splitdate_overlap)
Splitdate_overlap<-as.data.frame(Splitdate_overlap, stringsAsFactors=FALSE)
colnames(Splitdate_overlap)<-(c("year","month", "day"))
table(Splitdate_overlap$year)
