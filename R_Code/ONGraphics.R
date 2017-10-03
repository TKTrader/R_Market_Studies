ONGraphics <- function(dataCSV,graph1,graph2,graph3,graph4){
      data <- read.csv(dataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
      library(lubridate) ## for dates
      library(dplyr)
      ## IB Breaks
      IBHBreak <- filter(data, IBHBMFE>0)
      IBLBreak <- filter(data, IBLBMFE>0)
      IBBothBreak <- filter(data, BothBreak == 1)
      HBreak <- summary(IBHBreak$HBMFE)
      
      ## ON Breaks
      ONHBreak <- filter(data, ONHBMFE>0)
      ONHBreak <- filter(ONHBreak, Open < ONH)
      ONHBreak <- filter(ONHBreak, HBreak=="ONHIBH"|HBreak=="IBHONH"|HBreak=="ONH")
      ONLBreak <- filter(data, ONLBMFE>0)
      ONLBreak <- filter(ONLBreak, Open > ONL)
      ##dates <- unique(RTHdata$Date)   ## create list of dates
      RTH <- data.frame()
      library(ggplot2)
      library(scales)
      
      png(graph1,width=480,height=480)  ## create histogram of data
      dev.off()
      
      
}
a <- ggplot(IBHBreak,aes(x=HBreak,y=ONHBMFE))+geom_violin(fill="green")+geom_boxplot(fill="red",upper=.9,middle=.5,lower=.68,width=.15)+labs(title="ONH Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=20))
b <- ggplot(IBHBreak,aes(x=HBreak,y=ONHBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="ONH Break MFE(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=20))
bb <- ggplot(ONHBreak,aes(x=HBreak,y=ONHBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="ONH Break MFE(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_y_continuous(breaks=pretty_breaks(n=20))
c <- ggplot(IBLBreak,aes(x=LBreak,y=ONLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",middle=.5,upper=.9,lower=.68,width=.15)+labs(title="ONL Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=20))
d <- ggplot(ONLBreak,aes(x=LBreak,y=ONLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="ONL Break MFE (Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=20))
dd <- ggplot(ONLBreak,aes(x=LBreak,y=ONLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="ONL Break MFE (Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_y_continuous(breaks=pretty_breaks(n=20))

a <- ggplot(IBHBreak,aes(x=HBreak,y=Hdist))+geom_violin(fill="green")+geom_boxplot(fill="red",upper=.9,middle=.5,lower=.68,width=.15)+labs(title="ONH Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=20))
##UNCHANGED BELOW



## Time High X MFE x weekday  ## change legend color
aa <- ggplot(IBHBreak,aes(x=TimeHigh,y=HBMFE))+geom_point(aes(color=Weekday))+labs(title="Time MFE x MFE, IBH breaks",x="Time MFE High", y= "MFE")+
      scale_x_continuous(breaks=c(3,4,5,6,7,8,9,10,11), labels=c("10:30","11","11:30","12","12:30","1","1:30","2","2:30"))+ scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0,5))
bb <- ggplot(IBLBreak,aes(x=TimeLow,y=LBMFE))+geom_point(aes(color=Weekday))+labs(title="Time MFE x MFE, IBL breaks",x="Time MFE High", y= "MFE")+
      scale_x_continuous(breaks=c(3,4,5,6,7,8,9,10,11), labels=c("10:30","11","11:30","12","12:30","1","1:30","2","2:30"))+ scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0,5))
##filter out Wednesdays
IBLBreakW <- filter(IBLBreak, !grepl("4",Weekday))
IBHBreakW <- filter(IBHBreak, !grepl("4",Weekday))
e <- ggplot(IBHBreakW,aes(x=1,y=HBMFE))+geom_violin(fill="green")+geom_boxplot(fill="black",upper=.9,middle=.5,lower=.68,width=.15)+labs(title="Oil IBH Break MFE (Boxplot depicts 50/68/90%) No Wed",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
f <- ggplot(IBLBreakW,aes(x=1,y=LBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE (Quartile Boxplot, no Wed.)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))

## filter out by day of week
IBHBreakMonday <- filter(IBHBreak, Weekday==c("2"))
IBHBreakTuesday <- filter(IBHBreak, Weekday==c("3"))
IBHBreakWednesday <- filter(IBHBreak, Weekday==c("4"))
IBHBreakThursday <- filter(IBHBreak, Weekday==c("5"))
IBHBreakFriday <- filter(IBHBreak, Weekday==c("6"))
IBLBreakMonday <- filter(IBLBreak, Weekday==c("2"))
IBLBreakTuesday <- filter(IBLBreak, Weekday==c("3"))
IBLBreakWednesday <- filter(IBLBreak, Weekday==c("4"))
IBLBreakThursday <- filter(IBLBreak, Weekday==c("5"))
IBLBreakFriday <- filter(IBLBreak, Weekday==c("6"))

g <- ggplot(IBHBreakMonday,aes(x=1,y=HBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE Monday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
h <- ggplot(IBLBreakMonday,aes(x=1,y=LBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE Monday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
i <- ggplot(IBHBreakTuesday,aes(x=1,y=HBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE Tuesday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
j <- ggplot(IBLBreakTuesday,aes(x=1,y=LBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE Tuesday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
k <- ggplot(IBHBreakWednesday,aes(x=1,y=HBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE Wednesday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
l <- ggplot(IBLBreakWednesday,aes(x=1,y=LBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE Wednesday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
m <- ggplot(IBHBreakThursday,aes(x=1,y=HBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE Thursday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
n <- ggplot(IBLBreakThursday,aes(x=1,y=LBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE Thursday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
o <- ggplot(IBHBreakFriday,aes(x=1,y=HBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE Friday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
p <- ggplot(IBLBreakFriday,aes(x=1,y=LBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE Friday(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+