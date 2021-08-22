library(here)
library(epitools)


UOF <- read.csv(file=here('clean data/New Orleans/New Orleans UOF.csv'), stringsAsFactors = FALSE)


#referring to which variable you want to be the row and column of the odds ratio
OR_Function = function(rows,columns){
  #making a table of Level of force used with each race
  OR_Table<-table(rows, columns)
  print(OR_Table)
  
  ###Odds Ratio
  OR<-oddsratio(OR_Table)
  #printing the outcome so its easier to read
  print(OR$measure)
  print(OR$p.value)
  
}


OR_Function(UOF$Binning.Number.of.Officers,UOF$Force.Type)

