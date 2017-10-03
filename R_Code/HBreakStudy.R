HBreakStudy <- function(ONHlistCSV,ZigDataCSV,outputfilename,MFEoutputfilename){
  Data <- read.csv(ONHlistCSV, stringsAsFactors = FALSE)  ## import csv
  ZigData <- read.csv(ZigDataCSV, stringsAsFactors = FALSE)  ## import csv
  dates <- unique(ZigData$Date)   ## create list of dates
  HighBreaks <- data.frame() # create dataframe
  LowBreaks <- data.frame() ## create dataframe
  MFEfinalList <- data.frame()
  for (i in dates){
    Data <- Data[which(Data[,"Date"] %in% i),] ## split Data by date
    ZigData <- ZigData[which(ZigData[,"Date"] %in% i),] ## split ZigData by date
    fulldata <- cbind(Data, ZigData)
    FullDataUp <- fulldata[which(fulldata[,"Z.Dir"] %in% "UP"),] ## split Zig by UP, subtract .
    FullDataDown <- fulldata[which(fulldata[,"Z.Dir"] %in% "DOWN"),]  ## split zig by down
    FullDataUpRows <- nrow(FullDataUp)  ## number of Up rows
    FullDataDownRows <- nrow(FullDataDown)  ## number of down rows
    HBreaks <- FullDataUp[which(FullDataUp[,"ZzEnd"] > FullDataUp[,"ONH"]),]  ## find H Breaks
    LBreaks <- FullDataDown[which(FullDataDown[,"ZzEnd"] < FullDataDown[,"ONL"]),]  ## find L Breaks
    HBreaksRows <- nrow(HBreaks)  ## number of breaks
    MFEH <- data.frame() ## create dataframe
    for (i in 1:HBreaksRows){
      MFE <- HBreaks[i,"ZzEnd"]-HBreaks[i,"ONH"]  ## find MFE
      MFEH <- rbind(MFEH, MFE)  ## bind into list
    }
    colnames(MFEH) <- c("MFE") ## name MFE column
    MaxMFE <- round(max(Hbreaks2$MFE), digits = 4)  ## find max Break
    MinMFE <- round(min(Hbreaks2$MFE), digits = 4)   ## find min Break
    FinalHBreaks <- cbind(HBreaks,MFEH) ## bind data
    HighBreaks <- rbind(HighBreaks,FinalHBreaks) ## rbind data for each day
    MFElist <- cbind(MaxMFE,MinMFE)
    MFEfinalList <- rbind(MFEfinalList,MFElist)
 }
  write.csv(HighBreaks,file=outputfilename)  ## export as csv file to working directory,
  ## format = ("x.csv,'y')
  write.csv(MFEfinalList,file=MFEoutputfilename)
  }