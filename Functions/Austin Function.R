#set WD
setwd("C:/Users/katie/Desktop/policing/clean data/Austin")

#load in libraries
Citations<-read.csv("Citations_Austin.csv")
Shootings<-read.csv("Shootings_Austin.csv")
UOF<-read.csv("UseOfForce_Austin.csv")

#get list of races
listrace = unique(Citations$Subject.Race)

#make an empty matrix to put values in
db = matrix(ncol=length(listrace), nrow = length(Citations$Subject.Race))

#loop over the races to preform perfect match test and insert them into our matrix
for (i in 1:length(listrace)){
  db[,i] = Citations$Subject.Race ==listrace[i]
}

#get count of incidents
I=length(db[,1])

#get count of success
successes = table(db[,1])["TRUE"]

#perform test with hardcoded demographic value
binom.test(as.integer(successes), I, .34)
