#first I want to call libraries
library(dplyr)
library(stringr)
library(tidyverse)
library(epitools)

#in this script I want to make a dataset for shooting that has one row per officer ID so that it can be joined with citations


#set working directory
setwd("~/policing/policing/clean data/seattle")

#I want to call in my datasets
shooting<-read.csv(file='shootings_Seattle.csv', stringsAsFactors = TRUE)
shooting_SNs<-read.csv(file='shooting_SNs.csv', stringsAsFactors = TRUE)
shooting_sansrepeats<-distinct(shooting,shooting$GO, .keep_all=TRUE)
shootings_join<-left_join(shooting_sansrepeats, shooting_SNs, by="GO")


Officer.Serial<-as.integer(shootings_join$Officer.Serial)
shootings_join<-(cbind(shootings_join[,2:9], shootings_join[,16:28], Officer.Serial, shootings_join[,31:34]))

#FOR NOW I will just get rid of places where numbers of SNs per go =/= numbers of cases in shooting DB per go
#11-208425, 13-236273, 14-019975, 14-431136,  15-423533, 15-340293, 15-340351, 16-062644, and 16-10077
no_questionable_GOs<-shootings_join[!(shootings_join$GO=="11-208425" | shootings_join$GO=="13-236273" | shootings_join$GO=="14-019975" | shootings_join$GO=="14-431136" | shootings_join$GO=="15-423533" | shootings_join$GO=="15-340293" | shootings_join$GO=="15-340351" | shootings_join$GO=="16-062644" | shootings_join$GO=="16-10077"),]

citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = TRUE)


#why does officer ID change here?
officer_race_resolution<-as.data.frame(na.omit(cbind(as.character(citations$Subject.Perceived.Race), as.character(citations$Stop.Resolution), citations$Officer.Serial, citations$Officer.YOB, as.character(citations$Officer.Squad), as.character(citations$Officer.Race), citations$Officer.Serial, citations$name)))
officerrace<-as.data.frame(cbind(officer_race_resolution[,3:4], officer_race_resolution[6], officer_race_resolution$V8))
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
    OR_build_i<-cbind(nrow(citations_i), unique$V3[i], BW_OR$measure[2,1], BW_OR$measure[2,2], BW_OR$measure[2,3], BW_OR$p.value[2,3], unique$V4[i], unique$V6[i], unique$V7[i], unique$V8[i])
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
OR_table<-rename(OR_table, Officer.Serial = `officer ID`)
serial_int<-as.integer(OR_table$Officer.Serial)
OR_table$Officer.Serial<-serial_int
OR_table$`OR estimate`<-as.numeric(OR_table$`OR estimate`)


#Graph by officer of OR estimate histogram. See that most people have ORs around ~1.7, but some up around 12!
ggplot(OR_table, aes(`OR estimate`))+
  geom_histogram()

shootings_and_cites<-right_join(shootings_join, OR_table, by="Officer.Serial")

#add a column for whether or not the officer shot someone
for (i in 1:length(shootings_and_cites$GO)){
  if(is.na(shootings_and_cites$Date[i])){
    shootings_and_cites$shotyn[i]<-0
    
  }
  if  (!is.na(shootings_and_cites$Date[i])){
    shootings_and_cites$shotyn[i]<-1
  }
}

#add a column for whether or not the officer shot someone Black
for (i in 1:length(shootings_and_cites$shotyn)){
  shootings_and_cites$shotBlackyn[i]<-0
  if  (isTRUE(shootings_and_cites$Subject.Race[i]=="Black or African American")){
    shootings_and_cites$shotBlackyn[i]<-1
  }
}

#add a column for whether or not the officer shot someone White
for (i in 1:length(shootings_and_cites$shotyn)){
  shootings_and_cites$shotWhiteyn[i]<-0
  if  (isTRUE(shootings_and_cites$Subject.Race[i]=="White")){
    shootings_and_cites$shotWhiteyn[i]<-1
  }
}

cops_who_killed_Blacks<-shootings_and_cites[shootings_and_cites$shotBlackyn==1,]
mean_OR<-mean(as.numeric(cops_who_killed_Blacks$`OR estimate`))
cops_who_killed_Whites<-shootings_and_cites[shootings_and_cites$shotWhiteyn==1,]
mean_OR<-mean(as.numeric(cops_who_killed_Whites$`OR estimate`))
cops_who_didnt_kill<-shootings_and_cites[shootings_and_cites$shotyn==0,]
mean_OR<-mean(as.numeric(cops_who_didnt_kill$`OR estimate`))




#figure out distribution of precincts where shootings happened.
#first, create a DB that combines all citation data with all shootings data so that I can use precinct-level data from citation
add_precinct<-right_join(citations, shootings_join, by="Officer.Serial")
add_precinct<-distinct(add_precinct, add_precinct$Officer.Serial, .keep_all=TRUE)

#let's check out the odds ratios of different precincts
OR_and_precinct<-left_join(OR_table, citations, by="Officer.Serial")
OR_and_precinct<-distinct(OR_and_precinct,OR_and_precinct$Officer.Serial, .keep_all=TRUE)
OR_and_precinct$`OR estimate`<-(as.numeric(OR_and_precinct$`OR estimate`))
x<-aggregate(OR_and_precinct$`OR estimate`, list(OR_and_precinct$Precinct), mean)
    

for (i in 1:length(add_precinct)){
  if (add_precinct$Precinct[i]==NA & str_contains(add_precinct$Officer.Squad, "Nor")==TRUE
  
}
    
    