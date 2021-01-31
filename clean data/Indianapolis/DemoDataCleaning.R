#Set working directory
setwd("~/Desktop/GitAndR/Policing/dirty data/Indianapolis")

#Call in reference dataset
IndianapolisDemoData<-read.csv(file = 'IndianapolisDemoData.csv', stringsAsFactors = FALSE)

#Create data frame using vectors
Black<-c(0.286)
White<-c(0.609)
BiRacial<-c(0.033)
NA<-c(NA)
Hispanic<-c(0.105)
PacificIslander<-c(0.000)
Asian<-c(0.034)
NativeAmerican<-c(0.030)

#Combine vectors into data frame (UOF)
DemoDataUOF<-data.frame(Black, White, BiRacial, NA, Hispanic, PacificIslander, Asian, NativeAmerican)

#Combine vectors into data frame (OIS)
DemoDataOIS<-data.frame(Black, Hispanic, White, NA)

#Save
write.csv(DemoDataUOF,"~/Desktop/GitAndR/Policing/clean data/Indianapolis/DemoDataUOF.csv",row.names = FALSE)
write.csv(DemoDataOIS,"~/Desktop/GitAndR/Policing/clean data/Indianapolis/DemoDataOIS.csv",row.names = FALSE)




