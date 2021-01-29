#libraries
library(ggplot2)
library(here)
library(dplyr)

#WD
DemoData_UOF<-read.csv(file=here('clean data/orlando/Orlando City DemoData UOF.csv'), stringsAsFactors = FALSE)
DemoData_Shootings<-read.csv(file=here('clean data/orlando/Orlando City DemoData Shootings.csv'), stringsAsFactors = FALSE)

table(data2 <- data.frame(
  Group=c("White","Black","other"),
  Value=c("0.365","0.24","0.395")
  )
  )

ggplot(data2, aes(x="", y=Value, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)





