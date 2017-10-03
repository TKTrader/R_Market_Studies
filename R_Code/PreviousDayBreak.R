PreviousDayBreak <- function(RTHdataCSV,ONLfilename,ONHfilename){
      RTHdata <- read.csv(RTHdataCSV, header=TRUE, stringsAsFactors = FALSE)  ## import csv
      library(lubridate) ## for dates
      library(dplyr)
      library(scales)
      RTHdata <- mutate(RTHdata, YH = lag(High))
      RTHdata <- mutate(RTHdata, YL = lag(Low))
      RTHdata <- mutate(RTHdata, pRSI = lag(RSI))
      ## add volatility
      RTHdata <- RTHdata[-1,]  ## remove first row due to NA values
      dates <- unique(RTHdata$Date)   ## create list of dates
      RTH <- data.frame()
      for (i in dates){
            RTHdailydata <- RTHdata[which(RTHdata[,"Date"] %in% i),]
            H <- RTHdailydata$High ## find RTH High
            L <- RTHdailydata$Low ## find RTH Low
            YH <- RTHdailydata$YH
            YL <- RTHdailydata$YL
            O <- RTHdailydata$Open
            C <- RTHdailydata$Close
            pRSI <- RTHdailydata$pRSI
            if (H > YH && L < YL ){
                        Day <- c("Outside")  ##  Outside Day
                  } else if ((H<= YH) && (L >= YL)){
                        Day <- c("Inside") ## inside day
                  } else if ((H<= YH) && (L <= YL)){
                        Day <- c("LBreak") ## LOw BREAK
                  } else if ((H>= YH) && (L >= YL)){
                        Day <- c("HBreak") ## HIGH BREAK
                  }
            Range <- round(H - L, digits = 4) ## calculate range
            OCRange <- round(C - O, digits = 4)  ## find absolute value function
            RangePct <- round(OCRange/Range, digits = 3)  ## find proportion of ranges
            Weekday <- wday(mdy(i), label=TRUE) 
            if (O <= YH && O >= YL) {
                  Opening <- c("OAIR")
            } else if (O > YH){
                  Opening <- c("OAORA")
            } else if (O < YL){
                  Opening <- c("OAORB")
            } else {
                  Opening <- NA
            }
            RTHlist <- data.frame(i, Weekday, L, H, O, C, Day, OCRange, Range, RangePct, Opening,pRSI) ## add column with ONH, ONL, Range
            colnames(RTHlist) <- c("Date","Weekday", "L","H","O","C", "Day",  "OCRange", "Range", "RangePct", "opening","pRSI") ## name columns
            RTH <- rbind(RTH, RTHlist)
      }
      RTH16 <- RTH[1357:1449,] ## subset for 2016 only
      RTHOAIR <- filter(RTH, opening == "OAIR") ## subset for OAIR only
      RTH16OAIR <- filter(RTH16, opening == "OAIR") ## subset for OAIR only
      
      ## SUbset for winter wheat harvest
      ##RTH$date2 <- mdy(RTH$Date)
      RTH$month <-month(mdy(RTH$Date))
      ##RTH$DATE <- as.Date(RTH$Date)
      months <- c(5,6,7)
      RTHSprHar <- filter(RTH,month %in% months)
      RTHHarOAIR <- filter(RTHSprHar, opening == "OAIR") ## subset for OAIR only
      
      ## Subset for spring wheat harvest
      months2 <- c(8,9)
      RTHFallHar <- filter(RTH,month %in% months2)
      RTHFallOAIR <- filter(RTHFallHar, opening == "OAIR") ## subset for OAIR only
      
      ## subset for non harvest months
      months3 <- c(1,2,3,4,10,11,12)
      RTHNonHar <- filter(RTH,month %in% months3)
      RTHNonHarOAIR <- filter(RTHNonHar, opening == "OAIR") ## subset for OAIR only
      
      grid.arrange()
      
      h <- ggplot(RTH,aes(x=Day))+geom_bar(fill="black")+ labs(title="ZW RTH Day Type (Aug2010-Present)",x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      i <- ggplot(RTH16,aes(x=Day))+geom_bar(fill="green")+ labs(title="ZW RTH Day Type (2016)",x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      hh <- ggplot(RTHOAIR,aes(x=Day))+geom_bar(fill="black")+ labs(title="ZW RTH Day Type (Aug2010-Present) OAIR",x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      ii <- ggplot(RTH16OAIR,aes(x=Day))+geom_bar(fill="blue")+ labs(title="ZW RTH Day Type (2016) OAIR",x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      jj <- ggplot(RTH,aes(y=RSI,x=Day))+geom_violin(fill="red")+geom_point(fill="green")+labs(title="Daytype by previous day RSI",x="Day Type",y= "previous Daily RSI")+scale_y_continuous(breaks=pretty_breaks(n=20))
      kk <- ggplot(RTHOAIR,aes(y=RSI,x=Day))+geom_violin(fill="blue")+geom_point(fill="green")+labs(title="Daytype by previous day RSI, OAIR",x="Day Type",y= "previous Daily RSI")+scale_y_continuous(breaks=pretty_breaks(n=20))
      ll <- ggplot(RTHSprHar,aes(x=Day))+geom_bar(fill="dark blue")+ labs(title="ZW RTH Day Type Harvest (midMay-end July)", x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      mm <- ggplot(RTHHarOAIR,aes(x=Day))+geom_bar(fill="orange")+ labs(title="ZW RTH Day Type Harvest OAIR (midMay-end July)", x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      n <- ggplot(RTHFallHar,aes(x=Day))+geom_bar(fill="dark blue")+ labs(title="ZW RTH Day Type Harvest (Aug/Sept)", x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      oo <- ggplot(RTHFallOAIR,aes(x=Day))+geom_bar(fill="orange")+ labs(title="ZW RTH Day Type Harvest OAIR (Aug/Sept)", x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      p <- ggplot(RTHNonHar,aes(x=Day))+geom_bar(fill="dark green")+ labs(title="ZW RTH Day Type Non-Harvest (Oct-Apr)", x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      q <- ggplot(RTHNonHarOAIR,aes(x=Day))+geom_bar(fill="dark red")+ labs(title="ZW RTH Day Type Non-Harvest OAIR (Oct-Apr)", x="Day Type", y= "Frequency")+ scale_y_continuous(breaks=pretty_breaks(n=20))
      r <- ggplot(RTH,aes(x=pRSI,y=Range))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="pRSI vs Range",x="Previous Daily RSI",y= "Range")+
            scale_x_continuous(breaks=pretty_breaks(n=18),limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
      s <- ggplot(RTHOAIR,aes(x=pRSI,y=Range))+geom_point(fill="green")+labs(title="pRSI vs Range,OAIR",x="Previous Daily RSI",y= "Range")+
            scale_x_continuous(breaks=pretty_breaks(n=18),limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
      }