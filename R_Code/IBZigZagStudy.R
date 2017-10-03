## Zig Zag Study
## if issue check .0025 breaks in IBH/IBL look for zigzag not being setup on chart, lack of data
## creates NA/NAN value
setwd("C:/Users/User/datasciencecoursera/TradingPrograms") ## set working directory
IBZigBreak <- function(IBDataCSV,zigdataCSV,outputfilename){
      IBdata <- read.csv(IBDataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
      zigdata <- read.csv(zigdataCSV, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
      library(lubridate) ## for dates
      library(dplyr)
      library(ggplot2)
      library(scales)
      library(gtools)  ## for testing validity
      if (invalid(IBdata$Last)==FALSE){
            names(IBdata)[names(IBdata) == "Last"] <- "Close"
      }
      zigdata <- filter(zigdata,zzChange>0|zzChange<0) ## filter out zigs with 0 as value
      zigdata <- mutate(zigdata,zzStart = zzPrice-zzChange)  ## add zig start column 
      ## create list of IB Breaks
      IBHBreak <- filter(IBdata, HBMFE>0) ## filter for breaks
      IBLBreak <- filter(IBdata, LBMFE>0)  ## filter for breaks
      BothBreak <- filter(IBdata, HBMFE>0 & LBMFE>0)  ## filter for breaks
      datesIBH <- unique(IBHBreak$Date)   ## create list of dates
      datesIBL <- unique(IBLBreak$Date)  ## create list of dates
      datesBoth <- unique(BothBreak$Date)  ## create list of dates
      
      ## Date Formats can have issues
      datesIBH <- as.Date(datesIBH,"%m/%d/%Y")  ## match date formats of all data
      zigdata$Date <- as.Date(zigdata$Date,"%m/%d/%y")  ## check capitalization of Y!!!!!!!!!!!!!
      IBHBreak$Date <- as.Date(IBHBreak$Date,"%m/%d/%Y")
      datesIBH <- as.data.frame(datesIBH)
      names(datesIBH) <- c("Date")
      datesIBL <- as.Date(datesIBL,"%m/%d/%Y")  ## match date formats of all data
      IBLBreak$Date <- as.Date(IBLBreak$Date,"%m/%d/%Y")
      datesIBL <- as.data.frame(datesIBL)
      names(datesIBL) <- c("Date")
      datesBoth <- as.data.frame(datesBoth)
      names(datesBoth) <- c("Date")
      
      ## IBH Break Study
      datesIBH$match <- as.numeric(match(datesIBH$Date,zigdata$Date,nomatch=0))  ## match and subset columns
      datesIBH <- filter(datesIBH,match>0) 
      datesIBH <- datesIBH$Date
      IBHBreak <- mutate(IBHBreak,IBRange = IBH-IBL) ## find IB Range
      IBHBreak <- mutate(IBHBreak,IBRatio = round(HBMFE/IBRange,4))  ## have to change for contracts
      IBLBreak <- mutate(IBLBreak,IBRange = IBH-IBL) ## find IB Range
      IBLBreak <- mutate(IBLBreak,IBRatio = round(LBMFE/IBRange,4))
      a <- ggplot(IBHBreak,aes(x=IBRange,y=IBRatio))+geom_point(fill="green")+labs(title="IBH Break, IBRange vs IB Extension(2014-4/16)",x="IB Range", y= "% of IB extension")+
            scale_x_continuous(breaks=pretty_breaks(n=15))+scale_y_continuous(breaks=pretty_breaks(n=20),limits=c(0,8.5))+geom_hline(yintercept=c(0,1,2))+geom_smooth(method="lm")
      b <- ggplot(IBLBreak,aes(x=IBRange,y=IBRatio))+geom_point(fill="blue")+labs(title="IBL Break, IBRange vs IB Extension(2014-4/16)",x="IB Range", y= "% of IB extension")+
            scale_x_continuous(breaks=pretty_breaks(n=15),limits=c(0,2.0))+scale_y_continuous(breaks=pretty_breaks(n=20), limits=c(0,8.5))+geom_hline(yintercept=c(0,1,2))+geom_smooth(method="lm")
      aa <- ggplot(IBHBreak,aes(x=IBRange,y=Opening))+geom_point(fill="green")+labs(title="IBH Break, IBRange vs IB Extension(2014-4/16)",x="IB Range", y= "% of IB extension")+
            scale_x_continuous(breaks=pretty_breaks(n=15))+geom_hline(yintercept=c(0,1,2))+geom_smooth(method="lm")
       zigIBHdata <- data.frame() ## prepare empty dataframe for function
      ##Create function for finding MAE and MFE for each zigzag
      for (i in datesIBH){
            zigH <- zigdata[which(zigdata[,"Date"] == i),]
            IBHBreaksub <- IBHBreak[which(IBHBreak[,"Date"] == i),]
            q <- nrow(zigH)
            q <- c(1:q)
            zigH$IBH <- IBHBreaksub$IBH
            zigmicrodataH <- data.frame()
            for (p in q){
                  zigmicroH <- zigH[p,]
                  zigmicroH <- mutate(zigmicroH,MFE = round(max(zzStart,zzPrice) - IBH,4))
                  zigmicroH <- mutate(zigmicroH,MAE = round(min(zzStart,zzPrice) - IBH,4))
                  zigmicroH <- select(zigmicroH, -IBH)
                  zigmicrodataH <- rbind(zigmicroH,zigmicrodataH)
            }
            zigIBHdata <- rbind(zigmicrodataH,zigIBHdata)
            } ## success!!!
      ## create function for quantifying each zigzag by relationship to IB
            n <- nrow(zigIBHdata)
            n <- c(1:n)
            HBr <- data.frame() ## prepare empty function for loop
            for (i in n){
                  zigsubH <- zigIBHdata[i,]
                  if (zigsubH$zzChange > 0) && (zigsubH$MFE <=0){
                        zigsubH$Break <- 0  ## up zig is within IB
                  } else if ((zigsubH$zzChange > 0) && (zigsubH$MFE > 0) && (zigsubH$MAE < 0)){
                        zigsubH$Break <- 1   ## up zig breaks IBH
                  } else if ((zigsubH$zzChange > 0) && (zigsubH$MFE > 0) && (zigsubH$MAE >= 0)){
                        zigsubH$Break <- 2  ## up zig above IBH
                  } else if ((zigsubH$zzChange < 0) && (zigsubH$MFE <=0)){
                        zigsubH$Break <- 3  ## down zig within IB
                  } else if ((zigsubH$zzChange < 0) && (zigsubH$MFE > 0) && (zigsubH$MAE < 0)){
                        zigsubH$Break <- 4  ## down zig 
                  } else if ((zigsubH$zzChange < 0) && (zigsubH$MFE >0) && (zigsubH$MAE >= 0)){
                        zigsubH$Break <- 5}  ## down zig above IB
                  HBr <- rbind(zigsubH,HBr)
            }
      ## create function for extracting the first IB break of each day
            ## organize by TIME!!!!!!!!!!  ## problem!!!!!!!!!!!!!!zigdayHLength NA
            breakoutH <- data.frame()
            ZigBreakoutH <- data.frame()
            for (i in datesIBH){
                  zigdayH <- HBr[which(HBr[,"Date"] == i),]
                  IBHBreaksub <- IBHBreak[which(IBHBreak[,"Date"] == i),]
                  zigdayH <- zigdayH %>% distinct(Time)  ## eliminate duplicates
                  ##zigdayH$Time <- as.POSIXlt(zigdayH$Time,format="%H:%M:%S") ## format time
                  ##zigdayH <- zigdayH[order(zigdayH$Time , decreasing = FALSE ),]  ## order time
                  row.names(zigdayH) <- 1:nrow(zigdayH) ## renumber in order
                  ##zigdayH$Time <- format(zigdayH$Time,format="%H:%M:%S")  ## reformat into readable
                  zigdayHlength <- nrow(zigdayH)  ## number of zigs in day
                  one <- which(zigdayH$Break == 1) ## select rows of breakouts
                  if (invalid(one)==TRUE){
                        next
                  }
                  oneone <- one[1]  ## select 1st only
                  four <- which(zigdayH$Break == 4) ## select rows of breakins
                  ##invalid(four)  ## determine if 4 exists in dataframe (reentry into IB)
                  if (invalid(four)==TRUE){
                        
                        firstbreakoutH <- zigdayH[oneone:zigdayHlength,] ## select all rows after IB break (trend day)
                  } else if (invalid(four)==FALSE){
                        fourone <- four[1]  ## select first reentry into IB
                        firstbreakoutH <- zigdayH[oneone:fourone,]   ## select rows from break to reentry
                  }
                  Breakout1 <- firstbreakoutH[1,]   ## subset first breakout
                  Breakout1MFE <- Breakout1$MFE ## find MFE of first breakout
                  FirstPullback <- firstbreakoutH[2,] ## subset first pullback
                  ZigNumber <- nrow(firstbreakoutH) ## find length of breakout
                  FirstPullbackMAE <- FirstPullback$MAE  ## find MAE of first pullback
                  IBRange <- round(IBHBreaksub$IBRange,4)
                  HBMFE <- IBHBreaksub$HBMFE
                  RSI <- IBHBreaksub$RSI
                  MFEdif <- HBMFE - Breakout1MFE
                  Opening <- IBHBreaksub$Opening
                  ##BreakoutTime <- max(firstbreakout$Time)-min(firstbreakout$Time)  ## doesnt work
                  B <- data.frame(i,Breakout1MFE,FirstPullbackMAE,ZigNumber,IBRange,HBMFE,RSI,MFEdif,Opening)
                  breakoutH <- rbind(firstbreakoutH,breakoutH)  ## rbind all data together
                  ZigBreakoutH <-rbind(B,ZigBreakoutH)
            }
            
            c <- ggplot(ZigBreakoutH,aes(x=Breakout1MFE,y=FirstPullbackMAE))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="1st IBH Break MFE vs MAE (6t zig)",x="1st IB Break MFE", y= "MAE of first reentry into IB")+
                  scale_x_continuous(breaks=pretty_breaks(n=15))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
            d <- ggplot(ZigBreakoutH,aes(ZigNumber))+geom_histogram(fill="black",binwidth=1)+ labs(title="Number of Zigzags from IBH Break to IB Reentry (20t)",x="Number of Zigzags 1st IB Break", y= "Frequency")+scale_x_continuous(breaks=pretty_breaks(n=10))
            cc <-ggplot(ZigBreakoutH,aes(x=RSI,y=Breakout1MFE))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="pDaily RSI vs 1st IBH Break MFE",x="RSI", y= "1st IB Break MFE")+
                  scale_x_continuous(breaks=pretty_breaks(n=18), limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_smooth(method="lm")
            ccc <-ggplot(ZigBreakoutH,aes(x=RSI,y=HBMFE))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="pDaily RSI vs IBH Break MFE",x="RSI", y= "IB Break MFE")+
                  scale_x_continuous(breaks=pretty_breaks(n=18), limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_smooth(method="lm")
            cccc <-ggplot(ZigBreakoutH,aes(x=RSI,y=MFEdif))+geom_point(fill="blue")+labs(title="IBH Break, pDaily RSI vs MFE dif",x="RSI", y= "MFE dif (Day MFE-1st Break MFE)")+
                  scale_x_continuous(breaks=pretty_breaks(n=18), limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_smooth(method="lm")
            ZigBreakoutscale <- filter(ZigBreakoutH,Breakout1MFE >=.2)
            ZigBreakoutFailure <- filter(ZigBreakoutH,Breakout1MFE<.2)
            e<- ggplot(ZigBreakoutH,aes(x=IBRange,y=Breakout1MFE))+geom_point(fill="green")+labs(title="IBH Break, IBRange vs 1st IBH Break",x="IB Range", y= "1st IBH Break MFE (ticks)")+
                  scale_x_continuous(breaks=pretty_breaks(n=10))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_smooth(method="lm")
            f <- ggplot(IBHBreak,aes(x=IBRange,y=IBRatio))+geom_point(fill="blue")+labs(title="IBL Break, IBRange vs % Extension",x="IB Range", y= "% of IB extension")+
                  scale_x_continuous(breaks=pretty_breaks(n=10))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=c(0,1,2))+geom_smooth(method="lm")
            ##organize a chart by times IB broken
}
## Find second breakout after IB reentry
breakout2H <- data.frame()
ZigBreakoutH2 <- data.frame()
for (i in datesIBH){
      zigdayH2 <- HBr[which(HBr[,"Date"] == i),]
      IBHBreaksub2 <- IBHBreak[which(IBHBreak[,"Date"] == i),]
      ##zigday <- zigday %>% distinct(Time)  ## eliminate duplicates
      zigdayH2$Time <- as.POSIXlt(zigdayH2$Time,format="%H:%M") ## format time
      zigdayH2 <- zigdayH2[order(zigdayH2$Time , decreasing = FALSE ),]  ## order time
      zigdayH2$Row <- 1:nrow(zigdayH2) ## renumber in order
      ##zigdayH$Time <- format(zigdayH$Time,format="%H:%M:%S")  ## reformat into readable
      zigdayHlength2 <- nrow(zigdayH2)  ## number of zigs in day
      one2 <- which(zigdayH2$Break == 1) ## select rows of breakouts
      one2 <- one2[2]  ## select 2nd breakout only
      if (invalid(one2)==TRUE){
            breakoutH2 <- NA   ## fill data frame as empty
            } else if (invalid(one2)==FALSE){
            breakoutH2 <- zigdayH2[one2:zigdayHlength2,] ## select all rows after IB break (trend day)
            four2 <- which(breakoutH2$Break == 4) ## select rows of breakins
            if (invalid(four2)==FALSE){## determine if 4 exists in dataframe (reentry into IB)
                  four2 <- four2[1]  ## select first reentry into IB
                  breakoutH2 <- breakoutH2[1:four2,] ## select all rows after IB break (trend day)
                  Breakout2MFE <- breakoutH2$MFE  ## find MFE of first breakout
                  Pullback2 <- breakoutH2[2,] ## find MAE of first pullback
                  ZigNumber2 <- nrow(breakoutH2) ## find length of breakout
                  Pullback2MAE <- Pullback2$MAE
                  Date <- unique(breakoutH2$Date)
                  HBMFE2 <- IBHBreaksub2$HBMFE
                  ##BreakoutTime <- max(firstbreakout$Time)-min(firstbreakout$Time)  ## doesnt work
                  B <- cbind(i,Date, Breakout2MFE,Pullback2MAE,ZigNumber2,IBRange,HBMFE2)  ## IBrange and HBMFE2 wrong
                  breakout2H <- rbind(breakoutH2,breakout2H)  ## rbind all data together
                  ZigBreakoutH2 <-rbind(B,ZigBreakoutH2)
            } else if (invalid(four2)==TRUE){ ## no reentry into IB
                  Breakout2MFE <- max(breakoutH2$MFE)  ## find MFE of first breakout
                  Pullback2 <- breakoutH2[2,] ## subset to find MAE of first pullback
                  ZigNumber2 <- nrow(breakoutH2) ## find length of breakout
                  if (invalid(Pullback2)==TRUE){
                        Pullback2MAE <- max(breakoutH2$MFE)
                  } else if (invalid(Pullback2)==FALSE){
                        Pullback2MAE <- Pullback2$MAE
                  }
                  Date <- unique(breakoutH2$Date)
                  HBMFE2 <- IBHBreaksub2$HBMFE
                  ##BreakoutTime <- max(firstbreakout$Time)-min(firstbreakout$Time)  ## doesnt work
                  B <- cbind(i,Date, Breakout2MFE,Pullback2MAE,ZigNumber2,IBRange,HBMFE2)  ## IBrange and HBMFE2 wrong
                  breakout2H <- rbind(breakoutH2,breakout2H)  ## rbind all data together
                  ZigBreakoutH2 <-rbind(B,ZigBreakoutH2)
            }### reexamine charts in terms of "First Pullback"  Or first IB reentry, 2 charts
            ## check previous charts as well
            }
      g <- ggplot(ZigBreakoutH2,aes(x=Breakout2MFE,y=Pullback2MAE))+geom_point(fill="green")+labs(title="2nd IBH Break MFE vs MAE (6t zig)",x="2nd IB Break MFE", y= "MAE of first pullback")+
            scale_x_continuous(breaks=pretty_breaks(n=15))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
      h <- ggplot(ZigBreakoutH,aes(ZigNumber))+geom_histogram(fill="black",binwidth=1)+ labs(title="Number of Zigzags from IBH Break to IB Reentry (20t)",x="Number of Zigzags 1st IB Break", y= "Frequency")+scale_x_continuous(breaks=pretty_breaks(n=10))
      ##ZigBreakoutscale <- filter(ZigBreakoutH,Breakout1MFE >=.2)
      ##ZigBreakoutFailure <- filter(ZigBreakoutH,Breakout1MFE<.2)
      i<- ggplot(ZigBreakoutH,aes(x=IBRange,y=Breakout1MFE))+geom_point(fill="green")+labs(title="IBH Break, IBRange vs 1st IBH Break",x="IB Range", y= "1st IBH Break MFE (ticks)")+
            scale_x_continuous(breaks=pretty_breaks(n=10))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_smooth(method="lm")
      ## dataframe varables mixed up in following   ## FUCKED UP!!!!!!!!  issues with date
      ##RERUN OIL TO CHECK DATES!!!!!!!!
}

## IBL Break Study
datesIBL$match <- as.numeric(match(datesIBL$Date,zigdata$Date,nomatch=0))  ## match and subset columns
datesIBL <- filter(datesIBL,match>0) 
datesIBL <-datesIBL$Date
zigIBLdata <- data.frame() ## prepare empty dataframe for function
##Create function for finding MAE and MFE for each zigzag
for (i in datesIBL){
      zigL <- zigdata[which(zigdata[,"Date"] == i),]
      IBLBreaksub <- IBLBreak[which(IBLBreak[,"Date"] == i),]
      q <- nrow(zigL)
      q <- c(1:q)
      zigL$IBL <- IBLBreaksub$IBL
      zigmicrodataL <- data.frame()
      for (p in q){
            zigmicroL <- zigL[p,]
            zigmicroL <- mutate(zigmicroL,MFE = round(IBL-min(zzStart,zzPrice),4))
            zigmicroL <- mutate(zigmicroL,MAE = round(IBL-max(zzStart,zzPrice),4))
            zigmicroL <- select(zigmicroL, -IBL)
            zigmicrodataL <- rbind(zigmicroL,zigmicrodataL)
      }
      zigIBLdata <- rbind(zigmicrodataL,zigIBLdata)
} ## success!!!
## create function for quantifying each zigzag by relationship to IB
n <- nrow(zigIBLdata)
n <- c(1:n)
BrL <- data.frame() ## prepare empty function for loop
for (i in n){  #########  CHECK PARAMETERS!
      zigsubL <- zigIBLdata[i,]
      if (zigsubL$zzChange > 0 && zigsubL$MFE <=0){
            zigsubL$Break <- 3  ## up zig is within IB
      } else if ((zigsubL$zzChange > 0) && (zigsubL$MFE > 0) && (zigsubL$MAE < 0)){
            zigsubL$Break <- 4   ## up zig breaks IBL
      } else if ((zigsubL$zzChange > 0) && (zigsubL$MFE > 0) && (zigsubL$MAE >= 0)){
            zigsubL$Break <- 5  ## up zig outside IB
      } else if ((zigsubL$zzChange < 0) && (zigsubL$MFE <=0)){
            zigsubL$Break <- 0  ## down zig within IB
      } else if ((zigsubL$zzChange < 0) && (zigsubL$MFE > 0) && (zigsubL$MAE < 0)){
            zigsubL$Break <- 1  ## down zig breaks IBL
      } else if ((zigsubL$zzChange < 0) && (zigsubL$MFE >0) && (zigsubL$MAE >= 0)){
            zigsubL$Break <- 2}  ## down zig outside IB
      BrL <- rbind(zigsubL,BrL)
}
## create function for extracting the first IB break `of each day
## organize by TIME!!!!!!!!!!
library(gtools)  ## for testing validity 
breakoutL <- data.frame()
ZigBreakoutL <- data.frame()
for (i in datesIBL){ #### some arent breaks???????
      zigdayL <- BrL[which(BrL[,"Date"] == i),]
      IBLBreaksub <- IBLBreak[which(IBLBreak[,"Date"] == i),]  ## not registering
      ##zigday <- zigday %>% distinct(Time)  ## eliminate duplicates
      ##zigdayL$Time <- as.POSIXlt(zigdayL$Time,format="%H:%M:%S") ## format time
      ##zigdayL <- zigdayL[order(zigdayL$Time , decreasing = FALSE ),]  ## order time
      row.names(zigdayL) <- 1:nrow(zigdayL) ## renumber in order
      ##zigdayL$Time <- format(zigdayL$Time,format="%H:%M:%S")  ## reformat into readable
      zigdaylengthL <- nrow(zigdayL)  ## number of zigs in day
      oneL <- which(zigdayL$Break == 1) ## select rows of breakouts
      if (invalid(oneL)==TRUE){
            next
      }
      oneoneL <- oneL[1]  ## select 1st only       
      fourL <- which(zigdayL$Break == 4) ## select rows of breakins
      invalid(fourL)  ## determine if 4 exists in dataframe (reentry into IB)
      if (invalid(fourL)==TRUE){
            firstbreakoutL <- zigdayL[oneoneL:zigdaylengthL,] ## select all rows after IB break (trend day)
      } else if (invalid(fourL)==FALSE){
            fouroneL <- fourL[1]  ## select first reentry into IB
            firstbreakoutL <- zigdayL[oneoneL:fouroneL,]   ## select rows from break to reentry
      }
      
      ## dataframe variables mixed up in following
      
      Breakout1MFEL <- max(firstbreakoutL$MFE)  ## find MFE of first breakout
      FirstPullbackL <- firstbreakoutL[2,] ## find MAE of first pullback
      ZigNumberL <- nrow(firstbreakoutL) ## find length of breakout
      FirstPullbackMAEL <- FirstPullbackL$MAE
      IBRange <- IBLBreaksub$IBRange
      LBMFE <- IBLBreaksub$LBMFE
      RSIL <- IBLBreaksub$RSI
      MFEdifL <- LBMFE - Breakout1MFEL
      Opening <- IBLBreaksub$Opening
      ##BreakoutTime <- max(firstbreakout$Time)-min(firstbreakout$Time)  ## doesnt work
      B <- data.frame(i,Breakout1MFEL,FirstPullbackMAEL,ZigNumberL,IBRange,LBMFE,RSIL,MFEdifL,Opening)
      breakoutL <- rbind(firstbreakoutL,breakoutL)  ## rbind all data together
      ZigBreakoutL <-rbind(B,ZigBreakoutL)
}

j <- ggplot(ZigBreakoutL,aes(x=Breakout1MFEL,y=FirstPullbackMAEL))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="1st IBL Break MFE vs MAE",x="1st Break MFE(6t)",y= "MAE of first pullback")+
      scale_x_continuous(breaks=pretty_breaks(n=20))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
jj <- ggplot(ZigBreakoutL,aes(x=RSIL,y=Breakout1MFEL))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="1st IBL Break RSI vs MFE",x="Daily RSI",y= "1st Break MFE(6t)")+
      scale_x_continuous(breaks=pretty_breaks(n=18),limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
jjj <- ggplot(ZigBreakoutL,aes(x=RSIL,y=LBMFE))+geom_point(fill="green",aes(colour=factor(Opening)))+labs(title="IBL Break pDailyRSI vs MFE",x="Daily RSI",y= "MFE(6t)")+
      scale_x_continuous(breaks=pretty_breaks(n=18),limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=.2)+geom_smooth(method="lm")
jjjj <-ggplot(ZigBreakoutL,aes(x=RSIL,y=MFEdifL))+geom_point(fill="blue")+labs(title="IBL Break, pDaily RSI vs MFE dif",x="RSI", y= "MFE dif (Day MFE-1st Break MFE)")+
      scale_x_continuous(breaks=pretty_breaks(n=18), limits=c(10,90))+ scale_y_continuous(breaks=pretty_breaks(n=20))+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_smooth(method="lm")
k <- ggplot(ZigBreakoutL,aes(ZigNumberL))+geom_histogram(fill="black",binwidth=1)+ labs(title="Number of Zigzags from IBL Break to IB Reentry",x="Number of Zigzags 1st IB Break", y= "Frequency")+scale_x_continuous(breaks=pretty_breaks(n=10))
##ZigBreakoutscaleL <- filter(ZigBreakoutL,Breakout1MFE >=.2)
##ZigBreakoutFailureL <- filter(ZigBreakoutL,Breakout1MFE<.2)
l <- ggplot(ZigBreakoutH,aes(x=Opening,y=HBMFE))+geom_violin() + geom_point()
ll <- ggplot(ZigBreakoutH,aes(x=Opening,y=Breakout1MFE))+geom_violin() + geom_point()
m <- ggplot(ZigBreakoutL,aes(x=Opening,y=LBMFE))+geom_violin() + geom_point()
mm <- ggplot(ZigBreakoutL,aes(x=Opening,y=Breakout1MFEL))+geom_violin() + geom_point()
}
}
            
          