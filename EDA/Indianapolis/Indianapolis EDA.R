#First I want to call libraries
library(dplyr)
library(tidyr)
library(tidyverse)

#Set working directory
setwd("~/Desktop/GitAndR/Policing/EDA/Indianapolis")

#I want to call in my datasets (UOF)
UOF<-read.csv(file='UOF.csv', stringsAsFactors = TRUE)

