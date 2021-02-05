#libraries
library(ggplot2)
library(here)
library(dplyr)

#WD
DemoData_UOF<-read.csv(file=here('clean data/orlando/Orlando City DemoData UOF.csv'), stringsAsFactors = FALSE)
DemoData_Shootings<-read.csv(file=here('clean data/orlando/Orlando City DemoData Shootings.csv'), stringsAsFactors = FALSE)

table(data2 <- data.frame(
  Group=c("White","Black","other"),
  Value=c("278105","0.24","0.395")
  )
  )

ggplot(data2, aes(x="", y=Value, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start = 0)



data2 = data.frame(count=c(192770,61670,957,9084,50,13574), category=c("white","black", "Native America","Asian","Native Hawaiian","Mixed"))

  
data2$fraction = data2$count / sum(data2$count)
data2 = data2[order(data2$fraction), ]
data2$ymax = cumsum(data2$fraction)
data2$ymin = c(0, head(data2$ymax, n=-1))


ggplot(data2, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="Basic ring plot")





