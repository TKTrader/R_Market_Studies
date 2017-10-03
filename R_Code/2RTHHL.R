RTHHL <- function(RTHdataCSV,outputfilename){
  RTHdata <- read.csv(RTHdataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
  library(lubridate) ## for dates
  library(gtools)  ## for testing validity
  if (invalid(RTHdata$Last)==FALSE){
        names(RTHdata)[names(RTHdata) == "Last"] <- "Close"
  }
  RTH.time <-c("10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30")
  RTHdata <- filter(RTHdata, Time %in% RTH.time)
  dates <- unique(RTHdata$Date)   ## create list of dates
  RTH <- data.frame()
  for (i in dates){
    RTHdailydata <- RTHdata[which(RTHdata[,"Date"] %in% i),]
    H <- max(RTHdailydata$High) ## find RTH High
    TimeHigh <- which.max(RTHdailydata$High)
    L <- min(RTHdailydata$Low) ## find RTH Low
    TimeLow <- which.min(RTHdailydata$Low)
    Range <- round(H - L, digits = 4) ## calculate range
    Open <- RTHdailydata[1,"Open"]  ## calculate open of day
    Close <- RTHdailydata[nrow(RTHdailydata),"Close"] ## calculate Close of day
    OCRange <- round(Close - Open, digits = 4)  ## find absolute value function
    RangePct <- round(OCRange/Range, digits = 3)  ## find proportion of ranges
    Weekday <- wday(mdy(i), label=TRUE) 
    RTHlist <- data.frame(i, Weekday, L, TimeLow, H, TimeHigh, Open, Close, OCRange, Range, RangePct) ## add column with H, L, Range
    colnames(RTHlist) <- c("Date","Weekday", "L", "TimeLow", "H","TimeHigh", "Open", "Close", "OCRange", "Range", "RangePct") ## name columns
    RTH <- rbind(RTH, RTHlist)
  }
  write.csv(RTH,file=outputfilename)  ## export as csv file to working directory
  ## format = ("x.csv,'y')
}  ## find day of week for each date for stats, as.numeric for data?