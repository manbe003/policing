#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)
library(Policingfunctions)

#set working directory
setwd("C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/clean data/seattle")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = FALSE)
shootings<-read.csv(file='Shootings_Seattle.csv', stringsAsFactors = FALSE)


#I want to use a function I made that will spit out for each column: 1) how many NAs and 2) how many of those NAs are unique to the column (i.e. do not overlap with another column's NA)
UOF<-createNAChart(AllMetadata_UOF_Standardized)
Citations<-createNAChart(Metadata_Citations_Standardized)
shootings<-createNAChart(AllMetadata_shootings_clean)

#now I want to see histograms of join analysis

              