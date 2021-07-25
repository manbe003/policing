#loading libraries
library(here)
library(epitools)

#loading datasets
UOF<-read.csv(file=here('clean data/Northampton/Northampton UOF.csv'), stringsAsFactors = FALSE)

#making dataframe of force levels and officer group sizes
GroupSize_UOFLevel <- as.data.frame(na.omit(cbind(as.character(UOF$PD.Force.Type), as.character(UOF$Number.of.Officers))))

#making a table of Level of force used with each race
Level.Groups<-table(UOF$Number.of.Officers, UOF$PD.Force.Type)
print(Level.Groups)

###Odds Ratio
Size_OR<-oddsratio(Level.Groups)
#printing the outcome so its easier to read
print(Size_OR$measure)
print(Size_OR$p.value)
