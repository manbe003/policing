#set WD
setwd("C:/Users/katie/Desktop/policing/clean data/Austin")

#load in data
Citations<-read.csv("Citations_Austin.csv")
Shootings<-read.csv("Shootings_Austin.csv")
UOF<-read.csv("UseOfForce_Austin.csv")

for (i in 1:length(listrace)) {
  print(i)
}

for (i in 1:length(listrace)) {
  db[i] = Citations$Subject.Race == listrace[i]
}



racecol <- data.frame(Date=as.Date(character()),
                 File=character(), 
                 User=character(), 
                 stringsAsFactors=FALSE)




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
    print(binom.test(as.integer(successes), l, demodata[i]))
  }
}

FunctionAustin(demodata = Demodata, racecol = Citations$Subject.Race)



print(Demodata[i])



