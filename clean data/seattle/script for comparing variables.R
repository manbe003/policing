#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)
library(Policingfunctions)
library(tidyverse)

#set working directory
setwd("C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/clean data/seattle")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = FALSE)
shootings<-read.csv(file='Shootings_Seattle.csv', stringsAsFactors = FALSE)


#I want to use a function I made that will spit out for each column: 1) how many NAs and 2) how many of those NAs are unique to the column (i.e. do not overlap with another column's NA)
UOF_NAchart<-createNAChart(UOF)
Citations_NAchart<-createNAChart(citations)
shootings_NAchart<-createNAChart(shootings)

#now I want to see histograms of join analysis
UOFandCitations_officerID<-join_analysis(UOF$Officer_ID, citations$Officer.ID)
UOFandCitations_officerID[is.na(UOFandCitations_officerID)]<-0
hist(UOFandCitations_officerID$values.x,breaks=200)              
