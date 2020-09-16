#####################

#first I want to call libraries
library(dplyr)
library(tidyr)

#turn off scientific notation
options(scipen=999)

# change these two lines!
#I want to call in my datasets (use of force, shootings).
library(readr)
UOF <- read_csv("dirty data/orlando/OPD_Response_To_Resistance.csv")
shootings <- read_csv("dirty data/orlando/OPD_Officer-Involved_Shootings.csv")

#numericize the data (UOF)
#first split year/date into two separate variables
SplitDateTime_UOF<-strsplit(as.character(UOF$`Incident Date Time`),"\\s")
SplitDateTime_UOF<-do.call(rbind, SplitDateTime_UOF)
colnames(SplitDateTime_UOF)<-(c("date","time", "hour"))
SplitDateTime_UOF<-as.data.frame(SplitDateTime_UOF, stringsAsFactors=FALSE)

NoTitle_UOF <- UOF[UOF$`Incident Date Time` != "", ]

#making a table with all relevant metadata for UOF
AllMetadata_UOF<-cbind.data.frame(SplitDateTime_UOF$`date`,SplitDateTime_UOF$`time`,SplitDateTime_UOF$`hour`,NoTitle_UOF$`Officers Race`,NoTitle_UOF$`Officers Ethnicity`,NoTitle_UOF$`Officers Sex`,NoTitle_UOF$`Offenders Race`,NoTitle_UOF$`Offenders Ethnicity`,NoTitle_UOF$`Offenders Sex`, stringsAsFactors=FALSE)

colnames(AllMetadata_UOF)<-(c("date","time", "hour", "Officers.Race","Officers.Ethnicity","Officers.Sex", "Offenders.Race","Offenders.Ethnicity","Offenders.Sex"))

#make all null values = U for UOF
AllMetadata_UOF_U<-AllMetadata_UOF
AllMetadata_UOF_U[AllMetadata_UOF=="Not Specified"]<-"U"
AllMetadata_UOF_U[AllMetadata_UOF=="-"]<-"U"
AllMetadata_UOF_U[AllMetadata_UOF=="X"]<-"U"

#Change race, ethnicity, sex to be consistent with UOF
AllMetadata_UOF_Fix<-AllMetadata_UOF_U

AllMetadata_UOF_Fix$Officers.Race <- gsub('W', 'White', AllMetadata_UOF_Fix$Officers.Race)
AllMetadata_UOF_Fix$Officers.Race <- gsub('A', 'Asian', AllMetadata_UOF_Fix$Officers.Race)
AllMetadata_UOF_Fix$Officers.Race <- gsub('B', 'Black', AllMetadata_UOF_Fix$Officers.Race)
AllMetadata_UOF_Fix$Officers.Race <- gsub('U', 'Unknown', AllMetadata_UOF_Fix$Officers.Race)

AllMetadata_UOF_Fix$Officers.Ethnicity <- gsub('NH', 'Non-hispanic', AllMetadata_UOF_Fix$Officers.Ethnicity)
AllMetadata_UOF_Fix$Officers.Ethnicity <- gsub('HI', 'Hispanic', AllMetadata_UOF_Fix$Officers.Ethnicity)
AllMetadata_UOF_Fix$Officers.Ethnicity <- gsub('U', 'Unknown', AllMetadata_UOF_Fix$Officers.Ethnicity)

AllMetadata_UOF_Fix$Officers.Sex <- gsub('M', 'Male', AllMetadata_UOF_Fix$Officers.Sex)
AllMetadata_UOF_Fix$Officers.Sex <- gsub('F', 'Female', AllMetadata_UOF_Fix$Officers.Sex)
AllMetadata_UOF_Fix$Officers.Sex <- gsub('U', 'Unknown', AllMetadata_UOF_Fix$Officers.Sex)

AllMetadata_UOF_Fix$Offenders.Race <- gsub('W', 'White', AllMetadata_UOF_Fix$Offenders.Race)
AllMetadata_UOF_Fix$Offenders.Race <- gsub('A', 'Asian', AllMetadata_UOF_Fix$Offenders.Race)
AllMetadata_UOF_Fix$Offenders.Race <- gsub('B', 'Black', AllMetadata_UOF_Fix$Offenders.Race)
AllMetadata_UOF_Fix$Offenders.Race <- gsub('U', 'Unknown', AllMetadata_UOF_Fix$Offenders.Race)

AllMetadata_UOF_Fix$Offenders.Ethnicity <- gsub('NH', 'Non-hispanic', AllMetadata_UOF_Fix$Offenders.Ethnicity)
AllMetadata_UOF_Fix$Offenders.Ethnicity <- gsub('HI', 'Hispanic', AllMetadata_UOF_Fix$Offenders.Ethnicity)
AllMetadata_UOF_Fix$Offenders.Ethnicity <- gsub('U', 'Unknown', AllMetadata_UOF_Fix$Offenders.Ethnicity)

AllMetadata_UOF_Fix$Offenders.Sex <- gsub('M', 'Male', AllMetadata_UOF_Fix$Offenders.Sex)
AllMetadata_UOF_Fix$Offenders.Sex <- gsub('F', 'Female', AllMetadata_UOF_Fix$Offenders.Sex)
AllMetadata_UOF_Fix$Offenders.Sex <- gsub('U', 'Unknown', AllMetadata_UOF_Fix$Offenders.Sex)

#####################

#numericize the data (SHOOTINGS)
NoTitle_shootings <- shootings[shootings$Date != "", ]

#making a table with all relevant metadata for shootings
AllMetadata_shootings<-cbind.data.frame(NoTitle_shootings$Date,NoTitle_shootings$`Officer Race`,NoTitle_shootings$Ethnicity,NoTitle_shootings$`Officer Gender`,NoTitle_shootings$`Suspect Race`,NoTitle_shootings$`Suspect Gender`,NoTitle_shootings$`Suspect Hit`,NoTitle_shootings$Fatal, stringsAsFactors=FALSE)
colnames(AllMetadata_shootings)<-(c("date","Officer.Race","Officer.Ethnicity","Officer.Gender", "Suspect.Race","Suspect.Gender","Suspect.Hit","Fatal"))

#make all null values = U for shootings
AllMetadata_shootings_U<-AllMetadata_shootings
AllMetadata_shootings_U[AllMetadata_shootings=="n/a"]<-"U"
AllMetadata_shootings_U[AllMetadata_shootings=="Exempt"]<-"U"
AllMetadata_shootings_U[AllMetadata_shootings=="Undetermined"]<-"U"
AllMetadata_shootings_U[AllMetadata_shootings=="Omitted"]<-"U"

#Change race, ethnicity, sex to be consisted with shootings
AllMetadata_shootings_Fix<-AllMetadata_shootings_U

AllMetadata_shootings_Fix$Officer.Race <- gsub('W', 'White', AllMetadata_shootings_Fix$Officer.Race)
AllMetadata_shootings_Fix$Officer.Race <- gsub('A', 'Asian', AllMetadata_shootings_Fix$Officer.Race)
AllMetadata_shootings_Fix$Officer.Race <- gsub('B', 'Black', AllMetadata_shootings_Fix$Officer.Race)
AllMetadata_shootings_Fix$Officer.Race <- gsub('U', 'Unknown', AllMetadata_shootings_Fix$Officer.Race)
AllMetadata_shootings_Fix$Officer.Race <- gsub('O', 'Unknown', AllMetadata_shootings_Fix$Officer.Race)

AllMetadata_shootings_Fix$Officer.Ethnicity <- gsub('N', 'Non-hispanic', AllMetadata_shootings_Fix$Officer.Ethnicity)
AllMetadata_shootings_Fix$Officer.Ethnicity <- gsub('H', 'Hispanic', AllMetadata_shootings_Fix$Officer.Ethnicity)
AllMetadata_shootings_Fix$Officer.Ethnicity <- gsub('U', 'Unknown', AllMetadata_shootings_Fix$Officer.Ethnicity)
AllMetadata_shootings_Fix$Officer.Ethnicity <- gsub('W', 'White', AllMetadata_shootings_Fix$Officer.Ethnicity)
AllMetadata_shootings_Fix$Officer.Ethnicity <- gsub('A', 'Asian', AllMetadata_shootings_Fix$Officer.Ethnicity)

AllMetadata_shootings_Fix$Officer.Gender <- gsub('M', 'Male', AllMetadata_shootings_Fix$Officer.Gender)
AllMetadata_shootings_Fix$Officer.Gender <- gsub('F', 'Female', AllMetadata_shootings_Fix$Officer.Gender)
AllMetadata_shootings_Fix$Officer.Gender <- gsub('U', 'Unknown', AllMetadata_shootings_Fix$Officer.Gender)

AllMetadata_shootings_Fix$Suspect.Gender <- gsub('F', 'Female', AllMetadata_shootings_Fix$Suspect.Gender)
AllMetadata_shootings_Fix$Suspect.Gender <- gsub('M', 'Male', AllMetadata_shootings_Fix$Suspect.Gender)

#####################

#GENERATIVE DESCRIPTIVE DATA

summary(AllMetadata_UOF_Fix)
summary(AllMetadata_shootings_Fix)

#convert unknown values to zero (UOF) [optional because summary works already]
AllMetadata_UOF_Fix<-AllMetadata_UOF_Fix %>% replace(.=="NULL", 0)

#convert unknown values to zero (shootings)
AllMetadata_shootings_Fix<-AllMetadata_shootings_Fix %>% replace(.=="NULL", 0)

#DISPERSION DATA <- <- <- no numerical variables relevant [unneeded]

#############################
#JOIN DATA ANALYSIS

# The code below is purely for AllMetadata_UOF_Fix, for the shooting dataset the code is pretty similar aside from
# column names, use "," as separator, split string at ","

# chart 1 - grouped bar chart for the number of each race & ethnicity & gender of both officers and suspect/offenders
# data processing
UOFNumCount<-function(metadata){
  # paste cells into one string, use ";" as separator
  count.string <- paste(metadata, collapse = ";")
  # tackle the issue of blank column in the returned table
  count.string.clean <- gsub(";;", ";", count.string)
  # split string at ";"
  count.vector <- strsplit(count.string.clean, ";")[[1]]
  # get rid of white space to prevent errors
  count.vector.clean <- gsub(" ", "", count.vector)
  # tabulate data
  return(table(count.vector.clean))
}
# for officer
Officer_race_count <- UOFNumCount(AllMetadata_UOF_Fix$Officers.Race)
Officer_ethnicity_count <- UOFNumCount(AllMetadata_UOF_Fix$Officers.Ethnicity)
Officer_sex_count <- UOFNumCount(AllMetadata_UOF_Fix$Officers.Sex)
Officer_num_count <- merge(merge(Officer_race_count, Officer_ethnicity_count, all = TRUE), Officer_sex_count, all = TRUE)
# remove Unknown; what is the "I" in Officers.Race?
Officer_num_count <- Officer_num_count[!(Officer_num_count$count.vector.clean=="Unknown" | Officer_num_count$count.vector.clean=="I"),]
# for offender
Offender_race_count <- UOFNumCount(AllMetadata_UOF_Fix$Offenders.Race)
Offender_ethnicity_count <- UOFNumCount(AllMetadata_UOF_Fix$Offenders.Ethnicity)
Offender_sex_count <- UOFNumCount(AllMetadata_UOF_Fix$Offenders.Sex)
Offender_num_count <- merge(merge(Offender_race_count, Offender_ethnicity_count, all = TRUE), Offender_sex_count, all = TRUE)
Offender_num_count <- Offender_num_count[!Offender_num_count$count.vector.clean=="Unknown",]

# charting
library(ggplot2)
Officer_num_count$officer_or_offender <- "Officer"
Offender_num_count$officer_or_offender <- "Offender"

Both_num_count <- rbind(Officer_num_count,Offender_num_count)

# Grouped
ggplot(Both_num_count, aes(fill=officer_or_offender, y=Freq, x=count.vector.clean)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Race/Ethnicity/Sex") +
  ylab("Count") +
  guides(fill=guide_legend(title="Officer or Offender")) +
  # label each bar with number & adjusted label positions
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# chart 2 - create a unique code for each officer & offender based on their identities, bar chart comparison
# install.packages("purrr")
library("purrr")
library("rlist")

officer_code_list <- list()

for (row in 1:nrow(AllMetadata_UOF)) {
  race_list <- strsplit(AllMetadata_UOF[row, "Officers.Race"], ";")
  ethnicity_list  <- strsplit(AllMetadata_UOF[row, "Officers.Ethnicity"], ";")
  sex_list  <- strsplit(AllMetadata_UOF[row, "Officers.Sex"], ";")
  code <- map2(race_list, ethnicity_list, ~paste0(.x, .y))
  code <- map2(code, sex_list, ~paste0(.x, .y))
  
  for (each_code_vector in code) {
    for (each_code in each_code_vector) {
      officer_code_list <- list.append(officer_code_list, each_code)
    }
  }
}

officer_code_res <- data.frame(table(unlist(officer_code_list)))
officer_code_res$officer_or_offender <- "Officer"

offender_code_list <- list()

for (row in 1:nrow(AllMetadata_UOF)) {
  race_list <- strsplit(AllMetadata_UOF[row, "Offenders.Race"], ";")
  ethnicity_list  <- strsplit(AllMetadata_UOF[row, "Offenders.Ethnicity"], ";")
  sex_list  <- strsplit(AllMetadata_UOF[row, "Offenders.Sex"], ";")
  code <- map2(race_list, ethnicity_list, ~paste0(.x, .y))
  code <- map2(code, sex_list, ~paste0(.x, .y))
  
  for (each_code_vector in code) {
    for (each_code in each_code_vector) {
      offender_code_list <- list.append(offender_code_list, each_code)
    }
  }
}

offender_code_res <- data.frame(table(unlist(offender_code_list)))
offender_code_res$officer_or_offender <- "Offender"

# charting
Both_code_res <- rbind(officer_code_res, offender_code_res)

# Grouped
ggplot(Both_code_res, aes(fill=officer_or_offender, y=Freq, x=Var1)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Code") +
  ylab("Count") +
  guides(fill=guide_legend(title="Officer or Offender")) +
  # label each bar with number & adjusted label positions
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)