library(here)
library(epitools)
library(tidyverse)
library(dplyr)
library(tidyr)


UOF<- read.csv(file=here('clean data/Indianapolis/UOF.csv'), stringsAsFactors = FALSE)

UOF2<- group_by(UOF,id) 

distinct(UOF, officerIdentifier)

UOF2 <- UOF %>%
 dplyr::group_by(id) %>%
 summarise(officerIdentifier = paste(officerIdentifier, collapse=",  "))
UOF$officerIdentifier

UOF_IDFix <- UOF[, c("id","officerIdentifier")]

UOF_IDFix  <- for(i in 1:length(UOF_IDFix[,1])){
  if (UOF_IDFix[i,1] == i && UOF_IDFix[i,2] == i ) {
    
    UOF_IDFix[i,2][UOF_IDFix[i,2] == i ] <- "no"
    
  }}
