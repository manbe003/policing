#load libraries
library(here)
library(tidyverse)

#load datasets
Shootings<-read.csv(file=here('clean data/orlando/Shooting (cleaned).csv'), stringsAsFactors = FALSE)
UOF<-read.csv(file=here('clean data/orlando/UOF (cleaned).csv'), stringsAsFactors = FALSE)
DemoData_UOF<-read.csv(file=here('clean data/orlando/Orlando City DemoData UOF.csv'), stringsAsFactors = FALSE)
DemoData_Shootings<-read.csv(file=here('clean data/orlando/Orlando City DemoData Shootings.csv'), stringsAsFactors = FALSE)


#making people with white races and hispanic ethnicity have their race say hispanic
UOF_Offenders <- UOF
UOF_Offenders <-separate(UOF_Offenders, Offenders.Race, c('Offender one Race', 'Offender two Race', 'Offender three Race','Offender four Race', 'Offender five Race'), sep=";")
UOF_Offenders <-separate(UOF_Offenders, Offenders.Ethnicity, c('Offender one ethnicity', 'Offender two ethnicity', 'Offender three ethnicity','Offender four ethnicity', 'Offender five ethnicity'), sep=";")

UOF_Offenders<- within(UOF_Offenders, `Offender one Race`[`Offender one Race` == 'White' & `Offender one ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_Offenders<- within(UOF_Offenders, `Offender two Race`[`Offender two Race` == 'White' & `Offender two ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_Offenders<- within(UOF_Offenders, `Offender three Race`[`Offender three Race` == 'White' & `Offender three ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_Offenders<- within(UOF_Offenders, `Offender four Race`[`Offender four Race` == 'White' & `Offender four ethnicity` == 'Hispanic'] <- "Hispanic" )
UOF_Offenders<- within(UOF_Offenders, `Offender five Race`[`Offender five Race` == 'White' & `Offender five ethnicity` == 'Hispanic'] <- "Hispanic" )

#recombine them into one row to make groups
UOF_Offenders<- unite(UOF_Offenders,"Offenders.Race",`Offender one Race`:`Offender two Race`:`Offender three Race`:`Offender four Race`:`Offender five Race`, sep = ";", na.rm = TRUE, remove = FALSE)
UOF_Offenders<- unite(UOF_Offenders,"Offenders.Ethnicity",`Offender one ethnicity`:`Offender two ethnicity`:`Offender three ethnicity`:`Offender four ethnicity`:`Offender five ethnicity`, sep = ";", na.rm = TRUE, remove = FALSE)
UOF_Offenders<- UOF_Offenders[-c(10:14,16:20)]

#deleting rows where number of races and ethnicities dont match up.
UOF_Offenders<- UOF_Offenders[-c(3041, 4376, 5295, 1125, 3039, 5293, 4374), ]

#separating rows to be one offender per row
UOF_Offenders<- UOF_Offenders %>% separate_rows(Offenders.Race, sep= ";")
Shootings_Offenders<- Shootings %>% separate_rows(Suspect.Race, sep= ",")


##function
FunctionOrlando = function(demodata, racecol){
  x=colnames(racecol)
  listrace = unique(racecol)
  
  db = matrix(ncol=length(listrace), nrow = length(racecol))
  
  for (i in 1:length(listrace)){
    db[,i] = racecol ==listrace[i]
  }
  l=length(db[,i])
  
  for (i in 1:length(listrace)){
    successes = table(db[,i])["TRUE"]
    print(successes)
    print(listrace[i])
    print(binom.test(as.integer(successes), l, demodata[,i]))
  }
}

FunctionOrlando(demodata = DemoData_UOF, racecol = UOF_Offenders$Offenders.Race)
FunctionOrlando(demodata = DemoData_Shootings, racecol = Shootings_Offenders$Suspect.Race)




