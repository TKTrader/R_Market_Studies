HBreakStudy <- function(ONHlistCSV,ZigDataCSV,outputfilename,MFEoutputfilename){
  Data <- read.csv(ONHlistCSV, stringsAsFactors = FALSE)  ## import csv
  ZigData <- read.csv(ZigDataCSV, stringsAsFactors = FALSE)  ## import csv
  
  dates <- unique(ZigData$Date)   ## create list of dates
  finaldata <- data.frame() # create dataframe
  MFEfinalList <- data.frame()
  for (i in dates){
    Data <- Data[which(Data[,"Date"] %in% i),] ## split Data by date
    ZigData <- ZigData[which(ZigData[,"Date"] %in% i),] ## split ZigData by date
    fulldata <- data.frame(ZigData, Data)  ## join ONH data to zigdata   ## try iterating instead of cbind
    finaldata <- rbind(finaldata,fulldata)  ## rbind data into new dataset
    FullDataUp <- finaldata[which(finaldata[,"Z.Dir"] %in% "UP"),] ## split Zig by UP, subtract .
    FullDataDown <- fulldata[which(fulldata[,"Z.Dir"] %in% "DOWN"),]  ## split zig by down
    HBreaks <- FullDataUp[which(FullDataUp[,"ZzEnd"] > FullDataUp[,"ONH"]),]  ## find H Breaks
    FullDataUpRows <- nrow(FullDataUp)  ## number of Up rows
    FullDataDownRows <- nrow(FullDataDown)  ## number of down rows
    BreaksRows <- nrow(HBreaks)  ## number of breaks
    MFEH <- data.frame() ## create dataframe
    for (i in 1:BreaksRows){
      MFE <- HBreaks[i,"ZzEnd"]-HBreaks[i,"ONH"]  ## find MFE
      MFEH <- rbind(MFEH, MFE)  ## bind into list
    }
    colnames(MFEH) <- c("MFE") ## name MFE column
    MaxMFE <- round(max(MFEH$MFE), digits = 4)  ## find max Break
    MinMFE <- round(min(MFEH$MFE), digits = 4)   ## find min Break
    }  ## works!!! but not rbinding
    write.csv(HBreaks,file=outputfilename)
    write.csv(MaxMFE,file=outputfilename)
    write.csv(MinMFE,file=outputfilename)
    FinalHBreaks <- data.frame(HBreaks,MFEH) ## bind data
    HighBreaks <- rbind(HighBreaks,FinalHBreaks) ## rbind data for each day
    MFElist <- data.frame(MaxMFE,MinMFE)
    MFEfinalList <- rbind(MFEfinalList,MFElist)
    
    ## create new function, only MFE, not binding data
}