ONBreak <- function(RTHdataCSV,ONdataCSV,ONLfilename,ONHfilename){
  ONdata <- read.csv(ONdataCSV, header = TRUE, stringsAsFactors = FALSE)  ## import csv  ## transform to text?
  RTHdata <- read.csv(RTHdataCSV, header=TRUE, stringsAsFactors = FALSE)  ## import csv
  dates <- unique(ONdata$Date)   ## create list of dates
  ONLBreaklist <- data.frame() ## create list of ONL Breaks
  ONHBreaklist <- data.frame() ## create list of ONH breaks
  LBreakDateList <- data.frame()
  HBreakDateList <- data.frame()
  for (i in dates){
    RTHdailydata <- RTHdata[which(RTHdata[,"Date"] %in% i),] ## split RTH data by date
    ONdailydata <- ONdata[which(ONdata[,"Date"] %in% i),]  ## split ON data by date
    fulldata <- data.frame(ONdailydata,RTHdailydata) ### CHECK!!!!!!!!
    nolowgapdata <- fulldata[which(fulldata$Open > fulldata$ONL),] ##eliminate low gaps that break on Open
    nohighgapdata <- fulldata[which(fulldata$Open < fulldata$ONH),] ##eliminate high gaps that break on Open
    lowbreakdata <- nolowgapdata[which(nolowgapdata$L < nolowgapdata$ONL),]  ## works but issue
    ## if needs to be null. skip?
    ONLBreaklist <- rbind(ONLBreaklist,lowbreakdata)
    highbreakdata <- nohighgapdata[which(nohighgapdata$H > nolowgapdata$ONH),] 
    ONHBreaklist <- rbind(ONHBreaklist,highbreakdata)
  }
  write.csv(ONLBreaklist,file=ONLfilename)  ## export as csv file to working directory,
  ## format = ("x.csv,'y')
  write.csv(ONHBreaklist,file=ONHfilename)  ## export as csv file to working directory,
  ## format = ("x.csv,'y')
}