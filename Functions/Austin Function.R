#set WD
setwd("C:/Users/katie/Desktop/policing/clean data/Austin")

#load in data
Citations<-read.csv("Citations_Austin.csv")
Shootings<-read.csv("Shootings_Austin.csv")
UOF<-read.csv("UseOfForce_Austin.csv")
Demodata<-read.csv("Austin_City_DemoData.csv")

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
racecol<-as.data.frame

FunctionAustin = function(demodata, racecol){
  x=colnames(racecol)
  listrace = unique(racecol)
  
  db = matrix(ncol=length(listrace), nrow = length(racecol))
  
  for (i in 1:length(listrace)){
    db[,i] = racecol ==listrace[i]
  }
  l=length(db[,1])
  
  for (i in 1:length(listrace)){
    successes = table(db[,1])["TRUE"]
    print(successes)
    print(listrace[i])
    print(binom.test(as.integer(successes), l, demodata[,i]))
  }
}

FunctionAustin(demodata = Demodata, racecol = Citations$Subject.Race)


