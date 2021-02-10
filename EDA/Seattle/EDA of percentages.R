#library
install.packages(caTools)
library(caTools)

#read in files
shootings<-read.csv(file='shootings_Seattle.csv', stringsAsFactors = TRUE)
citations<-read.csv(file='citations_Seattle.csv', stringsAsFactors = TRUE)
UOF<-read.csv(file='UseOfForce_Seattle.csv', stringsAsFactors = TRUE)

#creating a graphic: shooting, UOF, citations per 100,000 people

#reduce duplicate entries
unique_citations<-citations[!duplicated(citations$Terry.Stop.ID),]
unique_shootings<-shootings[!duplicated(shootings$GO),]

#UOF first
population_UOF <- rbind(.003*753675, .151*753675, .07*753675, .066*753675,.006*753675, .645*753675)
rownames(population_UOF)<-cbind("Native American","Asian","Black","hispanic", "Pacific Islander", "White")

#split date into year
SplitDate_UOF<-strsplit(as.character(UOF$date),"/")
SplitDate_UOF<-do.call(rbind, SplitDate_UOF)
SplitDate_UOF<-as.data.frame(SplitDate_UOF, stringsAsFactors=FALSE)
colnames(SplitDate_UOF)<-(c("month","day", "year"))

UOF_yearsep<-cbind(SplitDate_UOF[,1:3], UOF[,2:12])
split<-split(UOF_yearsep, UOF_yearsep$year)

#raw number of UOF by race for each year
counts_UOF_race_year<-data.frame(matrix(vector(), 6, length(split)))
for (i in (1:length(split))){
t<-as.data.frame(table(split[[i]]$Subject_Race))
counts_UOF_race_year[i]<-t$Freq
rownames(counts_UOF_race_year)<-t$Var1
colnames(counts_UOF_race_year)<-unique(UOF_yearsep$year)
}
mean_UOF_2014_2019<-as.data.frame(rowMeans(counts_UOF_race_year[1:6]))

odds_per_100000_of_UOF<-(mean_UOF_2014_2019/population_UOF)*100000
colnames(odds_per_100000_of_UOF)<-x

barplot(odds_per_100000_of_UOF$x, names.arg = rownames(odds_per_100000_of_UOF))

#same thing with citations
#split date into year
SplitDate_citations<-strsplit(as.character(unique_citations$Reported.Date),"/")
SplitDate_citations<-do.call(rbind, SplitDate_citations)
SplitDate_citations<-as.data.frame(SplitDate_citations, stringsAsFactors=FALSE)
colnames(SplitDate_citations)<-(c("month","day", "year"))

citations_yearsep<-cbind(unique_citations[,1:12],SplitDate_citations[,1:3], unique_citations[,14:23])
split_citations<-split(citations_yearsep, citations_yearsep$year)

#raw number of shootings by race for each year
counts_citations_race_year<-data.frame(matrix(vector(), 7, length(split)))
for (i in (1:length(split_citations))){
  t<-as.data.frame(table(split_citations[[i]]$Subject.Perceived.Race))
  counts_citations_race_year[i]<-t$Freq
  rownames(counts_citations_race_year)<-t$Var1
}
colnames(counts_citations_race_year)<-unique(citations_yearsep$year)
mean_citations_2015_2019<-as.data.frame(rowMeans(counts_citations_race_year[1:5]))

population_citations <- rbind(.003*753675, .151*753675, .07*753675, .066*753675,.068*753675,.003*753675, .645*753675)
rownames(population_citations)<-cbind("Native American","Asian","Black","hispanic", "multiracial", "native hawaiian", "White")


odds_per_100000_of_citations<-(mean_citations_2015_2019/population_citations)*100000
colnames(odds_per_100000_of_citations)<-"x"

barplot(odds_per_100000_of_citations$x, names.arg = rownames(odds_per_100000_of_citations))

#same thing with shootings
#split date into year
SplitDate_shootings<-strsplit(as.character(unique_shootings$Date),"/")
SplitDate_shootings<-do.call(rbind, SplitDate_shootings)
SplitDate_shootings<-as.data.frame(SplitDate_shootings, stringsAsFactors=FALSE)
colnames(SplitDate_shootings)<-(c("month","day", "year"))

shootings_yearsep<-cbind(unique_shootings[,1:2],SplitDate_shootings[,1:3], unique_shootings[,4:28])
split_shootings<-split(shootings_yearsep, shootings_yearsep$year)

#raw number of shootings by race for each year
counts_shootings_race_year<-data.frame(matrix(vector(), 5, length(split)))
for (i in (1:length(split_shootings))){
  t<-as.data.frame(table(split_shootings[[i]]$Subject.Race))
  counts_shootings_race_year[i]<-t$Freq
  rownames(counts_shootings_race_year)<-t$Var1
}
colnames(counts_shootings_race_year)<-unique(shootings_yearsep$year)
mean_shootings_2005_2016<-as.data.frame(rowMeans(counts_shootings_race_year))

population_shootings <- rbind(.003*753675, .151*753675, .07*753675, .066*753675,.645*753675)
rownames(population_shootings)<-cbind("Native American","Asian","Black","hispanic",  "White")


odds_per_100000_of_shootings<-(mean_shootings_2005_2016/population_shootings)*100000
colnames(odds_per_100000_of_shootings)<-"x"

barplot(odds_per_100000_of_shootings$x, names.arg = rownames(odds_per_100000_of_shootings))
