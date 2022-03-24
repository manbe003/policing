#load dependencies and set working directory
source("ProjectPackageManagement.R")
source("Data Cleaning Functions.R")
PackageDependency()
setwd(here())


#load in data
Citations<-read.csv(file=here("clean data/Austin/Citations_Austin.csv"))
Shootings<-read.csv(file=here("clean data/Austin/Shootings_Austin.csv"))
UOF<-read.csv(file=here("clean data/Austin/UseOfForce_Austin.csv"))
Demodata_Citations<-read.csv(file=here("clean data/Austin/Austin_City_DemoData.csv"))
DemoData_UOF<-read.csv(file=here("clean data/Austin/Austin_City_DemoData_UOF.csv"))
DemoData_Shootings<-read.csv(file=here("clean data/Austin/Austin_City_DemoData_Shootings.csv"))

###Testing each part of the function separately

#get list of races
listrace = unique(Citations$Subject.Race)

#make an empty matrix to put values in
db = matrix(ncol=length(listrace), nrow = length(Citations$Subject.Race))

#loop over the races to preform perfect match test and insert them into our matrix
for (i in 1:length(listrace)){
  db[,i] = Citations$Subject.Race ==listrace[i]
}

#get count of incidents
l=length(db[,1])

#get count of success
successes = table(db[,1])["TRUE"]

#perform test with hardcoded demographic value
binom.test(as.integer(successes), l, .34)


#creating function


FunctionAustin = function(demodata, racecol){
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

FunctionAustin(demodata = na.omit (Demodata_Citations), racecol = na.omit(Citations$Subject.Race))
FunctionAustin(demodata = na.omit(DemoData_UOF), racecol = na.omit(UOF$subject.race))
FunctionAustin(demodata = DemoData_Shootings, racecol = Shootings$Subject.Race )


