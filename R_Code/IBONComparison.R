setwd("C:/Users/User/datasciencecoursera/TradingPrograms") ## set working directory
IBON <- function(RTHdataCSV,ONHLdataCSV,outputfilename){  ### need to eliminate gaps
      RTHdata <- read.csv(RTHdataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
      ONdata <- read.csv(ONHLdataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
      library(lubridate) ## for dates
      library(dplyr)
      library(gtools)  ## for testing validity
      if (invalid(RTHdata$Last)==FALSE){
            names(RTHdata)[names(RTHdata) == "Last"] <- "Close"
      }
      ## find common dates between 2 dataframes
      datesRTH <- unique(RTHdata$Date)   ## create list of dates
      datesON <- unique(ONdata$Date)
      datesboth <- intersect(datesRTH,datesON)
      RTH <- data.frame()
      for (i in datesboth){
            RTHdailydata <- RTHdata[which(RTHdata[,"Date"] %in% i),]
            ONHLdata <- ONdata[which(ONdata[,"Date"] %in% i),]
            ## Get H/L and ONH/ONL 
            H <- max(RTHdailydata$High) ## find RTH High
            TimeHigh <- which.max(RTHdailydata$High)
            ONH <- ONHLdata$ONH
            L <- min(RTHdailydata$Low) ## find RTH Low
            TimeLow <- which.min(RTHdailydata$Low)
            ONL <- ONHLdata$ONL
            ## Get IB values
            RTHIB <- (RTHdailydata)[1:2,] ## separate IB values
            IBH <- max(RTHIB$High) ## find IB High
            TimeIBH <- which.max(RTHIB$High)
            IBL <- min(RTHIB$Low) ## find IB Low
            TimeIBL <- which.min(RTHIB$Low)
            ## Calculate Distance btw ON/IB
            DistH <- abs(IBH-ONH)
            DistL <- abs(IBL-ONL)
            ## Get High and Low Breaks
            HB <- ifelse(H>IBH,1,0)
            LB <- ifelse(L<IBL,1,0)
            HBMFE <- ifelse(HB==1,round(H-IBH,4),0)
            LBMFE <- ifelse(LB==1,round(IBL-L,4),0)
            ## Get ONL and ONH Breaks
            ONHB <- ifelse(H>ONH,1,0)
            ONLB <- ifelse(L<ONL,1,0)
            ONHBMFE <- ifelse(ONHB==1,round(H-ONH,4),0)
            ONLBMFE <- ifelse(ONLB==1,round(ONL-L,4),0)
            ## Quantify day types by IB/ONHL
            x <- ifelse(IBH>ONH,1,0)
            y <- ifelse(IBL<ONL,1,0)
            xsum <- ONHB+HB
            ysum <- ONLB+LB
            if (x==1 & HB == 1 & ONHB==1){
                  D <- c("IBHONH") ## break ONH/IBH, IBH>ONH
            } else if (x==1 & HB==0  & ONHB==1){
                  D <- ("ONH")## break ONH not IBH, IBH?ONH
            } else if (x==0 & HB==1 & ONHB==1){
                  D <- c("ONHIBH") ## break ONH/IBH, IBH<=ONH
            } else if (x==0 & HB==1 & ONHB==0){
                  D <- c("IBH")  ## break IBH, IBH <=ONH
            } else if (HB==0 & ONHB==0){
                  D <- NA  ## no break
            } 
                  
            ## LOW BREAKS
            if (y==1 & LB == 1 & ONLB==1){
                  E <- c("ONLIBL") ##break both, IBL < ONL
            } else if (y==1 & LB==0  & ONLB==1){
                  E <- ("ONL") ## break ONL, IBL < ONL
            } else if (y==0 & LB==1 & ONLB==0){
                  E <- c("IBL") ## break IBL, IBL > ONL
            } else if (y==0 & LB==1 & ONLB==1){
                  E <- c("IBLONL") ## break IBL, IBL > ONL
            } else if (LB==0 & ONLB==0){
                  E <- NA  ## break neither
            }  
            
            ## NEED A BREAK BOTH WITH IBL > ONL????
                  
            Range <- round(H - L, digits = 4) ## calculate range
            IBRange <- round(IBH - IBL, digits = 4) ## calculate range
            Open <- RTHdailydata[1,"Open"]  ## calculate open of day
            Close <- RTHdailydata[nrow(RTHdailydata),"Close"] ## calculate Close of day ## change in Excel!
            OCRange <- round(Close - Open, digits = 4)  ## find absolute value function
            RangePct <- round(OCRange/Range, digits = 3)  ## find proportion of ranges
            Weekday <- wday(mdy(i), label=TRUE) 
            RTHlist <- data.frame(i, Weekday, L, TimeLow, H, TimeHigh, Open, Close, OCRange, Range, RangePct,IBH,IBL,TimeIBH,TimeIBL, HB, LB,HBMFE,LBMFE,D,E,ONH,ONHBMFE,ONL,ONLBMFE,DistH,DistL) ## add column with ONH, ONL, Range
            colnames(RTHlist) <- c("Date","Weekday", "L", "TimeLow", "H","TimeHigh", "Open", "Close", "OCRange", "Range", "RangePct","IBH","IBL","TimeIBH","TimeIBL","IBHBreak","IBLBreak","HBMFE","LBMFE","HBreak","LBreak","ONH","ONHBMFE","ONL","ONLBMFE","DistH","DistL") ## name columns
            RTH <- rbind(RTH, RTHlist)
      }
      RTH <- mutate(RTH,OpenAbove = Open - lag(H))
      RTH <- mutate(RTH,OpenBelow = lag(L) - Open)
      RTH2 <- data.frame()
      for (i in datesboth){
            RTHdailydata2 <- RTH[which(RTH[,"Date"] %in% i),]
            if (is.na(RTHdailydata2$OpenAbove)==TRUE){
                  Opening <- NA
            } else if (RTHdailydata2$OpenAbove > 0){
                  Opening <- c("OAORA")
            } else if ((RTHdailydata2$OpenAbove <0) && (RTHdailydata2$OpenBelow > 0)){
                  Opening <- c("OAORB")
            } else {
                  Opening <- c("OAIR")
            }
            RTHsub <- data.frame(RTHdailydata2,Opening)
            colnames(RTHsub) <- c("Date","Weekday", "L", "TimeLow", "H","TimeHigh", "Open", "Close", "OCRange", "Range", "RangePct","IBH","IBL","TimeIBH","TimeIBL","IBHBreak","IBLBreak","HBMFE","LBMFE","HBreak","LBreak","ONH","ONHBMFE","ONL","ONLBMFE","OpenAbove","OpenBelow","Opening")
            RTH2 <- rbind(RTH2, RTHsub)
      }
      write.csv(RTH2,file=outputfilename)  ## export as csv file to working directory
      ## format = ("x.csv,'y')
}  ## find day of week for each date for stats, as.numeric for data?