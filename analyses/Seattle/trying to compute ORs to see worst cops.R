#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)
library(epitools)

#set working directory
setwd("/Users/katherine/Policing/clean data/seattle")

#I want to call in my datasets (citations).
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = TRUE)

#count the number of of each race with a citation or arrest vs field contact or offense report (then try w/o offense report)
#make a column that includes both race AND result
officer_race_resolution<-as.data.frame(na.omit(cbind(as.character(citations$Subject.Perceived.Race), as.character(citations$Stop.Resolution), citations$Officer.ID, citations$Officer.YOB, as.character(citations$Officer.Squad), as.character(citations$Officer.Race))))
officerrace<-as.data.frame(cbind(officer_race_resolution[,3:4], officer_race_resolution[6]))
citations_raceandresolution<-as.data.frame(na.omit(cbind(as.character(officer_race_resolution$V1), as.character(officer_race_resolution$V2))))
citations_uniteraceandresolution<-unite(citations_raceandresolution,race_resolution,sep=", ")
citations_withunitedcol<-cbind(officer_race_resolution$V3, citations_uniteraceandresolution)

OR_table<-data.frame(matrix(NA, ncol=8, nrow=length(unique(citations_withunitedcol$`officer_race_resolution$V3`))))
colnames(OR_table)<-c("total instances", "officer ID", "OR estimate", "lower 95% CI", "upper 95% CI", "chi square significance test", "officer YOB", "officer race")
blackbetter<-0
blackworse<-0
whitebetter<-0
whiteworse<-0

#how many of each instance does each officer have?
for (i in 1:length(unique(citations_withunitedcol$`officer_race_resolution$V3`))){
#t/f of which rows are the corect level
  unique<-unique(officerrace)
  x<-citations_withunitedcol$`officer_race_resolution$V3`==unique$V3[i]
  table(x)
#add to df
  citations_i<-as.data.frame(cbind(citations_withunitedcol$`officer_race_resolution$V3`, citations_withunitedcol$race_resolution, x))
  
#remove falses
  citations_i[citations_i==FALSE]<-NA
  citations_i<-na.omit(citations_i)
  

#how many of each instance?
counts<-as.data.frame(table(citations_i$V2))
counts_raceasrowname<-as.data.frame(counts$Freq) 
rownames(counts_raceasrowname)<-counts$Var1

#put into 2 by 2 matrix for each officer
#first compute total number of arrests, citations, refer to prosecution
sum_black_worse<-sum(counts_raceasrowname["Black or African American, Arrest",],counts_raceasrowname["Black or African American, Citation / Infraction",],counts_raceasrowname["Black or African American, Referred for Prosecution",],na.rm=TRUE)
sum_black_better<-sum(counts_raceasrowname["Black or African American, Field Contact",],counts_raceasrowname["Black or African American, Offense Report",], na.rm=TRUE)
sum_white_better<-sum(counts_raceasrowname["White, Field Contact",],counts_raceasrowname["White, Offense Report",], na.rm=TRUE)
sum_white_worse<-sum(counts_raceasrowname["White, Arrest",],counts_raceasrowname["White, Citation / Infraction",],counts_raceasrowname["White, Referred for Prosecution",], na.rm=TRUE)


BW<-matrix(c(sum_white_better, sum_black_better, sum_white_worse, sum_black_worse), ncol=2)
BW[BW==0]<-NA

rownames(BW)<-c("white", "black")
colnames(BW)<-c("contact_only", "cite/arrest")

if(any(is.na(BW))!=TRUE){
  
  BW_OR<-oddsratio(BW)
  OR_build_i<-cbind(nrow(citations_i), unique$V3[i], BW_OR$measure[2,1], BW_OR$measure[2,2], BW_OR$measure[2,3], BW_OR$p.value[2,3], unique$V4[i], unique$V6[i])
  OR_table[i,]<-OR_build_i
}

if (is.na(BW[1,1])){
  whitebetter<-whitebetter+1
}
if (is.na(BW[1,2])){
  whiteworse<-whiteworse+1
}
if (is.na(BW[2,1])){
  blackbetter<-blackbetter+1
}
if (is.na(BW[2,2])){
  blackworse<-blackworse+1
}

}

OR_table<-na.omit(OR_table)

#when they are missing one value, is there any pattern in what is missing?
