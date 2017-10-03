IB <- function(RTHdataCSV,outputfilename){
  RTHdata <- read.csv(RTHdataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
  library(lubridate) ## for dates
  library(dplyr)
  if (invalid(RTHdata$Last)==FALSE){
        names(RTHdata)[names(RTHdata) == "Last"] <- "Close"
  }
  dates <- unique(RTHdata$Date)   ## create list of dates
  RTH <- data.frame()
  for (i in dates){
    RTHdailydata <- RTHdata[which(RTHdata[,"Date"] %in% i),]
    H <- max(RTHdailydata$High) ## find RTH High
    TimeHigh <- which.max(RTHdailydata$High)
    L <- min(RTHdailydata$Low) ## find RTH Low
    TimeLow <- which.min(RTHdailydata$Low)
    RTHIB <- (RTHdailydata)[1:2,] ## separate IB values
    IBH <- max(RTHIB$High) ## find IB High
    TimeIBH <- which.max(RTHIB$High)
    IBL <- min(RTHIB$Low) ## find IB Low
    TimeIBL <- which.min(RTHIB$Low)
    HB <- ifelse(H>IBH,1,0)
    LB <- ifelse(L<IBL,1,0)
    HBMFE <- ifelse(HB==1,round(H-IBH,4),0)
    LBMFE <- ifelse(LB==1,round(IBL-L,4),0)
    Range <- round(H - L, digits = 4) ## calculate range
    IBRange <- round(IBH - IBL, digits = 4) ## calculate range
    Open <- RTHdailydata[1,"Open"]  ## calculate open of day
    Close <- RTHdailydata[nrow(RTHdailydata),"Close"] ## calculate Close of day
    OCRange <- round(Close - Open, digits = 4)  ## find absolute value function
    RangePct <- round(OCRange/Range, digits = 3)  ## find proportion of ranges
    Weekday <- wday(mdy(i), label=TRUE) 
    RTHlist <- data.frame(i, Weekday, L, TimeLow, H, TimeHigh, Open, Close, OCRange, Range, RangePct,IBH,IBL,TimeIBH,TimeIBL, HB, LB,HBMFE,LBMFE) ## add column with ONH, ONL, Range
    colnames(RTHlist) <- c("Date","Weekday", "L", "TimeLow", "H","TimeHigh", "Open", "Close", "OCRange", "Range", "RangePct","IBH","IBL","TimeIBH","TimeIBL","IBHBreak","IBLBreak","HBMFE","LBMFE") ## name columns
    RTH <- rbind(RTH, RTHlist)
  }
  RTH <- mutate(RTH,GapAbove = Open - lag(H))
  RTH <- mutate(RTH,GapBelow = lag(L) - Open)
  RTH2 <- data.frame()
  for (i in dates){
        RTHdailydata2 <- RTH[which(RTH[,"Date"] %in% i),]
        if (is.na(RTHdailydata2$GapAbove)==TRUE){
              Opening <- NA
        } else if (RTHdailydata2$GapAbove > 0){
              Opening <- c("OAORA")
        } else if ((RTHdailydata2$GapAbove <0) && (RTHdailydata2$GapBelow > 0)){
              Opening <- c("OAORB")
        } else {
              Opening <- c("OAIR")
        }
        RTHsub <- data.frame(RTHdailydata2,Opening)
        colnames(RTHsub) <- c("Date","Weekday", "L", "TimeLow", "H","TimeHigh", "Open", "Close", "OCRange", "Range", "RangePct","IBH","IBL","TimeIBH","TimeIBL","IBHBreak","IBLBreak","HBMFE","LBMFE","GapAbove","GapBelow","Opening")
        RTH2 <- rbind(RTH2, RTHsub)
        }
  write.csv(RTH2,file=outputfilename)  ## export as csv file to working directory
  ## format = ("x.csv,'y')
}  ## find day of week for each date for stats, as.numeric for data?