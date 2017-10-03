IBGraphics <- function(IBdataCSV,graph1,graph2,graph3,graph4){
      IBdata <- read.csv(IBdataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
      library(lubridate) ## for dates
      library(dplyr)
      IBHBreak <- filter(data, IBHBMFE>0)
      IBLBreak <- filter(data, IBLBMFE>0)
      IBBothBreak <- filter(data, BothBreak == 1)
      HBreak <- summary(IBHBreak$HBMFE)
      ##dates <- unique(RTHdata$Date)   ## create list of dates
      RTH <- data.frame()
      library(ggplot2)
      library(scales)
      png(graph1,width=480,height=480)  ## create histogram of data
      dev.off()
      
      
}
### GRAINS
## Range - IB Break MFE
a <- ggplot(IBHBreak,aes(x=IB.range,y=IBHBMFE))+geom_point(fill="green")+labs(title="IB.Range vs IBH Break MFE",x="IB Range",y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
b <- ggplot(IBLBreak,aes(x=IB.range,y=IBLBMFE))+geom_point(fill="green")+labs(title="IB.Range vs IBL Break MFE",x="IB Range",y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
## RSI -  IB Break MFE
c <- ggplot(IBHBreak,aes(x=p.RSI,y=IBHBMFE))+geom_point(fill="green")+labs(title="p.RSI vs IBH Break MFE",x="p.RSI",y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
d <- ggplot(IBLBreak,aes(x=p.RSI,y=IBLBMFE))+geom_point(fill="green")+labs(title="p.RSI vs IBL Break MFE",x="p.RSI",y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
## RSI -  IB Break MFE.prop
e <- ggplot(IBHBreak,aes(x=p.RSI,y=HBMFE.prop))+geom_point(fill="green")+labs(title="p.RSI vs IBH Break MFE.prop",x="p.RSI",y= "MFE.prop")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
f <- ggplot(IBLBreak,aes(x=p.RSI,y=LBMFE.prop))+geom_point(fill="green")+labs(title="p.RSI vs IBL Break MFE.prop",x="p.RSI",y= "MFE.prop")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
## IBL Break vs IBH Break on Both
g <- ggplot(IBBothBreak,aes(x=LBMFE.prop,y=HBMFE.prop))+geom_point(fill="green")+labs(title="IBL Break MFE.prop vs IBH Break MFE.prop",x="IBL Break",y= "IBH Break")+
          scale_x_continuous(breaks=pretty_breaks(n=18))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_smooth(method="lm")
h <- ggplot(IBBothBreak,aes(x=IBLBMFE,y=IBHBMFE))+geom_point(fill="green")+labs(title="IBL Break MFE vs IBH Break MFE",x="IBL Break",y= "IBH Break")+
          scale_x_continuous(breaks=pretty_breaks(n=20))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_smooth(method="lm")

##6-21-16



a <- ggplot(IBHBreak,aes(x=1,y=IBHBMFE))+geom_violin(fill="green")+geom_boxplot(fill="black",upper=.9,middle=.5,lower=.68,width=.15)+labs(title="Oil IBH Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
b <- ggplot(IBHBreak,aes(x=1,y=IBHBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
c <- ggplot(IBLBreak,aes(x=1,y=IBLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",middle=.5,upper=.9,lower=.68,width=.15)+labs(title="Oil IBL Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
d <- ggplot(IBLBreak,aes(x=1,y=IBLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE (Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
          scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))




### OIL
a <- ggplot(IBHBreak,aes(x=1,y=IBHBMFE))+geom_violin(fill="green")+geom_boxplot(fill="black",upper=.9,middle=.5,lower=.68,width=.15)+labs(title="Oil IBH Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
b <- ggplot(IBHBreak,aes(x=1,y=IBHBMFE))+geom_violin(fill="green") +geom_boxplot(fill="black",width=.15)+labs(title="IBH Break MFE(Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=40), limits=c(0,3))
c <- ggplot(IBLBreak,aes(x=1,y=IBLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",middle=.5,upper=.9,lower=.68,width=.15)+labs(title="Oil IBL Break MFE (Boxplot depicts 50/68/90%)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))
d <- ggplot(IBLBreak,aes(x=1,y=IBLBMFE))+geom_violin(fill="red")+geom_boxplot(fill="black",width=.15)+labs(title="Oil IBL Break MFE (Quartile Boxplot)",x="Relative Frequency", y= "MFE")+
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))

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
      scale_x_continuous(breaks=pretty_breaks(n=0))+ scale_y_continuous(breaks=pretty_breaks(n=30), limits=c(0,3))


## MFE half hour
q <- ggplot(IBHBreak,aes(TimeHigh))+geom_histogram(fill="black",binwidth=.5)+ labs(title="Oil IBH Break MFE, Half Hour reached ",x="Half Hour MFE reached", y= "Frequency")+scale_x_continuous(breaks=c(3,4,5,6,7,8,9,10,11), labels=c("10:30","11","11:30","12","12:30","1","1:30","2","2:30"))
r <- ggplot(IBLBreak,aes(TimeLow))+geom_histogram(fill="black",binwidth=.45)+ labs(title="Oil IBL Break MFE, Half Hour reached ",x="Half Hour MFE reached", y= "Frequency")+scale_x_continuous(breaks=c(3,4,5,6,7,8,9,10,11),labels=c("10:30","11","11:30","12","12:30","1","1:30","2","2:30"))

## need to find time of first breaks!
Create labels for quartile
annotate("text", x=8, y=13000, label= "boat")