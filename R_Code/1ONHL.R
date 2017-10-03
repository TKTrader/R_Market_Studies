setwd("C:/Users/User/datasciencecoursera/TradingPrograms") ## set working directory
## feed in ful data of 30 minute bars, Grains; make sure time formatted properly
ONHL <- function(ONdataCSV,outputfilename){
  ONdata <- read.csv(ONdataCSV, stringsAsFactors = FALSE)  ## import csv
  library(gtools)  ## for testing validity
  library(lubridate)
  # change "Last" column to "Close"
  if (invalid(ONdata$Last)==FALSE){
        names(ONdata)[names(ONdata) == "Last"] <- "Close"
  }
  ONdata$Date <- mdy(ONdata$Date) ## convert Date format
  ## create function for adding date to ON 
  y.time <- c("19:30","20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30") ## times date need alteration
  r.time <-c("0:00","0:30","1:00","1:30","2:00","2:30","3:00","3:30","4:00","4:30","5:00","5:30","6:00","6:30","7:00","7:30","8:00","8:30","9:00")
  r <- filter(ONdata, Time %in% y.time)
  q <- filter(ONdata, Time %in% r.time)
  r$Date <- r$Date + days(1)
  ONdata <- rbind(r,q)
  dates <- unique(ONdata$Date)   ## create list of dates
  ONHL <- data.frame()  
  for (i in dates){
    dailydata <- ONdata[which(ONdata[,"Date"] == i),]  ## split data by date
    ONH <- max(dailydata$High) ## find ONH
    ONL <- min(dailydata$Low) ## find ONL
    d <- as.character(unique(dailydata$Date))
    ONRange <- round(ONH - ONL, digits = 4) ## calculate range
    ONOpen <- dailydata[1,"Open"]  ## calculate open
    ONClose <- dailydata[nrow(dailydata),"Close"] ##calculate Close of day
    OCONRange <- abs(round(ONClose - ONOpen, digits = 4))  ## find range
    ONRangePct <- round(OCONRange/ONRange, digits = 3)  ## find proportion of ranges
    dailydataON <- data.frame(d, ONOpen, ONClose, ONL, ONH, OCONRange, ONRange, ONRangePct)  ## add column with ONH, ONL, Range
    colnames(dailydataON) <- c("Date","ONOpen", "ONClose", "ONL", "ONH", "OCONRange", "ONRange", "ONRangePct") ## name columns
    ONHL <- rbind(ONHL, dailydataON)  ## rbind list of subsets
 }
  write.csv(ONHL,file=outputfilename)  ## export as csv file to working directory,
  ## format = ("x.csv,'y')
}  ## find day of week for each date for stats, as.numeric for data?