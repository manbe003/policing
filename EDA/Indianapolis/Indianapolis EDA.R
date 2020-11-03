#First I want to call libraries
library(dplyr)
library(tidyr)
library(tidyverse)

#Set working directory
setwd("~/Desktop/GitAndR/Policing/EDA/Indianapolis")

#I want to call in my datasets (OIS, UOF)
OIS<-read.csv(file = 'Indianapolis Officer Involved Shootings.csv', stringsAsFactors = FALSE)
UOF<-read.csv(file = 'Indianapolis Use of Force Incidents.csv', stringsAsFactors = FALSE)

