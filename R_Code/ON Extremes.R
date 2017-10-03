data <- read.csv("ZWU5ON30min.csv", stringsAsFactors = FALSE)  ## import csv
                 dates <- unique(data$Date)   ## create list of dates
                 datesATR <- dates[21:length(dates)]  ## create list of dates for 20D ATR
                 ## strptime(data$Date, format="%d/%m/%Y")   ## not used
                 ## rownames <- data$Date  ## not used
                  ## CREATE FUNCTION JUST FOR ON EXTREMELIST AND TEST
ONExtreme <- data.frame()  
for i in dates { ## not working - ON data
  dailydata <- data[which(data[,"Date"] %in% i),] ## separate data into rows of each data
  ONH <- max(dailydata$High) ## find maximum
  ONL <- min(dailydata$Low) ## find minimum
  ONRange <- round(ONH - ONL, digits = 4) ## calculate range, (check rounding decimals!!!)
  dailydataON <- cbind(dailydata, ONH, ONL, ONRange)  ## add column with ONH, ONL, Range
  ONExtreme <- rbind(ONExtreme, dailydataON)  ## rbind list of subsets
  ONList <- as.data.frame(cbind(dates, ONL, ONH, ONRange))  ## check rounding decimals
} ## code works, but failure on overall function

write.csv(ONExtreme,file='whatever.csv')  ## export as csv file to working directory

RTHdata <- read.csv("ZWU5Day.csv", stringsAsFactors = FALSE)  ## import csv
RTH <- data.frame()
for i in dates{
  RTHdailydata <- RTHdata[which(RTHdata[,"Date"] %in% i),]
  H <- max(RTHdailydata$High) ## find maximum
  L <- min(RTHdailydata$Low) ## find minimum
  Range <- round(H - L, digits = 4) ## calculate range, (check rounding decimals!!!)
  RTHlist <- as.data.frame(cbind(dates, H, L, Range)) ## add column with ONH, ONL, Range
  RTH <- rbind(RTH, RTHlist)
}

FinalDailyList <- cbind(ONList, RTHlist)  ## create composite of ON and RTH data
ONLBreaklist <- data.frame() ## create list of ONL Breaks
ONHBreaklist <- data.frame() ## create list of ONH breaks
for i in dates{
  breakdata <- FinalDailyList[which(FinalDailyList[,"Date"] %in% i),] ## separate data into rows of each data
  if (breakdata$L < breakdata$ONL){
    ONLDatelist <- rbind(ONLBreaklist,breakdata)
  }
    if (breakdata$H > breakdata$ONH){
      ONHDateList <- rbind(ONHbreaklist,breakdata)
    } 
}

##from new break lists, calculate MFE


if H < YC, ATR = YC - L  ##change to use ATRDates
if L > YC, ATR = H - YC
else ATR = H - L
20DATR <- mean()

colnames(ONExtrema) <- c("dates","ONL","ONH")
rangelist <- data.frame()
for i in dates {
  dailydata <- ONExtrema[which(ONExtrema[,"dates"] %in% i),] ## separate data into rows of each data
  range <- dailydata$ONH - dailydata$ONL ## find maxima
  rangelist <- rbind(rangelist,range)  ## rbind list of ONHs
  colnames(rangelist) <- c("range")
} ## not working
library(xlsx)  ### export csv
write.xlsx(mydata, "c:/mydata.xlsx")  ## export csv


ONExtrema <- cbind(dates,ONLList,ONHList,rangelist)

data$newdate <- strptime(as.character(data$Date), "%d/%m/%Y")
format(data$newdate, "%m-%d-%Y")