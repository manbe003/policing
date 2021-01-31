#Call libraries
library(here)
library(dplyr)

#Set working directory
setwd(here("clean data/Indianapolis"))

#Import data
OIS = read.csv("OIS.csv")
UOF = read.csv("UOF.csv")
DemoDataOIS = read.csv("DemoDataOIS.csv")
DemoDataUOF = read.csv("DemoDataUOF.csv")

#Checking Distinct Counts
#x=unique(UOF$residentAge)
y = distinct(UOF,id,useOfForceReason,arrestMade,residentInjured,residentHospitalized,residentRace,residentSex,residentAge)
z = distinct(UOF, id)

#Create list of races
listrace = unique(UOF$residentRace)

#Create empty matrix
db = matrix(ncol = length(listrace), nrow = length(UOF$residentRace))

#Create loop to insert race into empty matrix
for(i in 1:length(listrace)){
  db[,i] = UOF$residentRace == listrace[i]
}

#Create count  of incidents
l = length(db[,1])

#Create count of successes
successes = table(db[,1])["TRUE"]

#Perform test with hardcoded demographic value
binom.test(as.integer(successes), l, 0.5)

#Create function
IndianapolisFunction = function(demodata, racecol){
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

IndianapolisFunction(demodata = IndianapolisDemoData, racecol = UOF$residentRace)
IndianapolisFunction(demodata = IndianapolisDemoData, racecol = OIS$residentRace)
  