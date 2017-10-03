setwd("C:/Users/User/datasciencecoursera/TradingPrograms") ## set working Directory   ## watch D replacement!
## feeD in full Data of 30 minute bars, Grains; make sure time formatteD properly
DailyLevels <- function(DataCSV,DailyRSIcsv,Dailyvolcsv,ON.csv,output.csv){
  data <- read.csv(DataCSV, stringsAsFactors = FALSE,header=TRUE)  ## import csv
  data2 <- read.csv(DataCSV, stringsAsFactors = FALSE,header=TRUE)  ## import csv
  data.rsi <- read.csv(DailyRSIcsv, stringsAsFactors = FALSE,header=TRUE)  ## import csv
  data.vol <- read.csv(Dailyvolcsv, stringsAsFactors = FALSE,header=TRUE)  ## import csv
  
  library(gtools)  ## for testing validity
  library(lubridate)
  library(dplyr)
  # change "Last" column to "Close"
  if (invalid(data$Last)==FALSE){
        names(data)[names(data) == "Last"] <- "Close"
  }
  if (invalid(data2$Last)==FALSE){
            names(data2)[names(data2) == "Last"] <- "Close"
  }
  data$Date <- mdy(data$Date) ## convert date format
  data2$Date <- mdy(data2$Date)
  data.rsi$Date <- mdy(data.rsi$Date)
  data.vol$Date <- mdy(data.vol$Date)
  ## create function for adding date to ON 
  y.time <- c("19:30","20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30") ## times date need alteration
  r.time <-c("0:00","0:30","1:00","1:30","2:00","2:30","3:00","3:30","4:00","4:30","5:00","5:30","6:00","6:30","7:00","7:30","8:00","8:30","9:00")
  r <- filter(data, Time %in% y.time)
  q <- filter(data, Time %in% r.time)
  r$Date <- r$Date + days(1)
  data <- rbind(r,q)
  dates <- unique(data$Date)   ## create list of dates
  ONHL <- data.frame()  
  ## find ON levels
  for (i in dates){
    DailyData <- data[which(data[,"Date"] == i),]  ## split data by date
    ONH <- max(DailyData$High) ## find ONH
    ONL <- min(DailyData$Low) ## find ONL
    d <- as.character(unique(DailyData$Date))
    ONRange <- round(ONH - ONL, digits = 4) ## calculate range
    ONOpen <- DailyData[1,"Open"]  ## calculate open
    ONClose <- DailyData[nrow(DailyData),"Close"] ##calculate Close of day
    OCONRange <- abs(round(ONClose - ONOpen, digits = 4))  ## find range
    OCONRange.prop <- round(OCONRange/ONRange, digits = 3)  ## find proportion of ranges
    DailyDataON <- data.frame(d, ONOpen, ONClose, ONL, ONH, OCONRange, ONRange, OCONRange.prop)  ## add column with ONH, ONL, Range
    colnames(DailyDataON) <- c("Date","ONOpen", "ONClose", "ONL", "ONH", "OCONRange", "ONRange", "OCONRange.prop") ## name columns
    ONHL <- rbind(ONHL, DailyDataON)  ## rbind list of subsets
  }
  write.csv(ONHL,file=ON.csv)  ## export as csv file to working directory
  ## Find RTH Levels
  RTH.time <-c("10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30")
  RTHData <- filter(data2, Time %in% RTH.time)
  dates.RTH <- unique(RTHData$Date)   ## create list of dates
  RTH <- data.frame()
  for (i in dates.RTH){
            RTHDailyData <- RTHData[which(RTHData[,"Date"] == i),]
            RTHDailyRSI <- data.rsi[which(data.rsi[,"Date"] == i),]
            RTHDailyVOL <- data.vol[which(data.vol[,"Date"] == i),]
            H <- max(RTHDailyData$High) ## find RTH High
            TimeHigh <- which.max(RTHDailyData$High)
            L <- min(RTHDailyData$Low) ## find RTH Low
            TimeLow <- which.min(RTHDailyData$Low)
            Range <- round(H - L, digits = 4) ## calculate range
            if (Range == 0){
                      next
            }  ## eliminate non-trading dates with range = 0
            Open <- RTHDailyData[1,"Open"]  ## calculate open of day
            Close <- RTHDailyData[nrow(RTHDailyData),"Close"] ## calculate Close of day
            OCRange <- abs(round(Close - Open, digits = 4))  ## find absolute value function
            Range.prop <- round(OCRange/Range, digits = 3)  ## find proportion of ranges
            D <- as.character(unique(RTHDailyData$Date))
            RTHIB <- (RTHDailyData)[1:2,] ## separate IB values
            IBH <- max(RTHIB$High) ## find IB High
            TimeIBH <- which.max(RTHIB$High)
            IBL <- min(RTHIB$Low) ## find IB Low
            TimeIBL <- which.min(RTHIB$Low)
            IB.range <- round(IBH - IBL,digits=4)
            IB.range.prop <- round(Range/IB.range,2)
            HB <- ifelse(H>IBH,1,0)
            LB <- ifelse(L<IBL,1,0)
            BothB <-ifelse(HB==1 & LB == 1,1,0)
            HBMFE <- ifelse(HB==1,round(H-IBH,4),0)
            LBMFE <- ifelse(LB==1,round(IBL-L,4),0)             ## colnames() necessary????  
            HBMFE.prop <- round(HBMFE/IB.range,2)
            LBMFE.prop <- round(LBMFE/IB.range,2)
            if (invalid(RTHDailyRSI)==TRUE){
                      RSI <- NA
                      rsi.comp.fast <- NA
                      rsi.comp.med <- NA
                      rsi.comp.slow <- NA
                      volume <- NA
            } else {RSI <- round(RTHDailyRSI$RSI,1)
            volume <- RTHDailyRSI$VOL
            rsi.comp.fast <- round(RTHDailyRSI$rsi.comp.fast,1)
            rsi.comp.med <- round(RTHDailyRSI$rsi.comp.med,1)
            rsi.comp.slow <- round(RTHDailyRSI$rsi.comp.slow)}
            
            if (invalid(RTHDailyVOL)==TRUE){
                      vol <- NA
                      vol.delta <- NA
            } else if (invalid(RTHDailyVOL)==FALSE){
                      vol <- RTHDailyVOL$Open
                      vol.delta <- RTHDailyVOL$Last-RTHDailyVOL$Open
                      }
            RTHlist <- data.frame(D, L, TimeLow, H, TimeHigh, Open, Close, OCRange, Range, Range.prop,IBH,IBL,TimeIBH,TimeIBL,IB.range,IB.range.prop, HB, LB, BothB, HBMFE,HBMFE.prop,LBMFE,LBMFE.prop,RSI,rsi.comp.fast,rsi.comp.med,rsi.comp.slow,vol,vol.delta,volume) ## add column with H, L, Range
            colnames(RTHlist) <- c("Date","L", "TimeLow", "H","TimeHigh", "Open", "Close", "OCRange", "Range", "Range.prop","IBH","IBL","TimeIBH","TimeIBL","IB.range","IB.range.prop","IBHBreak","IBLBreak","BothBreak","IBHBMFE","HBMFE.prop","IBLBMFE","LBMFE.prop","RSI","rsi.comp.fast","rsi.comp.med","rsi.comp.slow","Volatility","vol.delta","volume") ## name columns
            RTH <- rbind(RTH, RTHlist)
  }
  ## Use switch function to replace 0's with NA
  RTH$RSI[RTH$RSI==0] <- NA
  RTH$rsi.comp.fast[RTH$rsi.comp.fast==0] <- NA
  RTH$rsi.comp.med[RTH$rsi.comp.med==0] <- NA
  RTH$rsi.comp.slow[RTH$rsi.comp.slow==0] <- NA
  
  ## find previous day's H/L and daytype, break YH/L, IBH/L
  RTH <- RTH %>% mutate(YH = lag(H)) %>% mutate(YL = lag(L)) %>% mutate(GapAboveYH = Open - YH)
  RTH <- RTH %>% mutate(GapBelowYL = YL - Open) %>% mutate(BreakYL = YL - L) %>% mutate(BreakYH = H - YH)
  RTH <- RTH %>% mutate(Opening = ifelse(GapAboveYH>0,"OAORA",(ifelse(GapBelowYL>0, "OAORB","OAIR"))))
  RTH <- RTH %>% mutate(p.RSI = lag(RSI)) %>% mutate(p.vol.delta = lag(vol.delta)) %>% mutate(volume.delta = volume - (lag(volume)))
  RTH <- RTH %>% mutate(p.volume = lag(volume)) %>% mutate(p.volume.delta = lag(volume.delta)) %>% mutate(p.IB.range.prop = lag(IB.range.prop))
  RTH <- RTH %>% mutate(p.rsi.comp.fast = lag(rsi.comp.fast)) %>% mutate(p.rsi.comp.med = lag(rsi.comp.med)) %>% mutate(p.rsi.comp.slow = lag(rsi.comp.slow))
  RTH <- RTH %>% mutate(weekday = wday(Date)) %>% mutate(RSI.delta = RSI-p.RSI) %>% mutate(p.RSI.delta = lag(RSI.delta)) ## create function for oil EIA days

  ##mav <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}  ## function for calculating moving average
  ## Calculate ATR.20
  library(TTR)
  RTH <- RTH %>% mutate(ATR.20 = round(SMA(RTH$Range,n=20),4)) %>% mutate(range.delta = Range-lag(Range)) %>% mutate(range.ATR.delta = Range - ATR.20)
  RTH <- RTH %>% mutate(p.range.ATR.delta = lag(range.ATR.delta))
  
  ##RTH$match <- as.numeric(match(RTH$Date,ONHL$Date,nomatch=0))  ## match and subset columns
  ##HBreakList2 <- filter(HBreakList,match>0) 
  
  ##merge RTH and ON dataframes  CHECK VALUES!!!
  fulldata <- merge(RTH, ONHL, by=c("Date"),all=TRUE)  ## will have dates that dont match!!!!!!! use complete.cases?
  fulldata <- fulldata %>% mutate(ONHB = H - ONH) %>% mutate(ONLB = ONL - L) %>% mutate(BreakYH = H - YH)
  fulldata <- fulldata %>% mutate(IBH.ONH = ifelse(IBH > ONH,1,0)) %>% mutate(IBL.ONL = ifelse(IBL < ONL,1,0))
  RTH <- RTH %>% mutate(ON.RTH.HB = ifelse(ONH > YH,1,0)) %>% mutate(ON.RTH.LB = ifelse(ONL < YL, 1,0)) %>% mutate(ON.RTH.HBMFE = ONH - YH)
  RTH <- RTH %>% mutate(ON.RTH.HBMFE.prop = round((ONH - YH)/lag(Range),1)) %>% mutate(ON.RTH.LBMFE = YL - ONL) %>% mutate(ON.RTH.LBMFE.prop = round((YL - ONL)/lag(Range),1)) 
  RTH <- RTH %>% mutate(prop.RTH.C.dist.H = round((H-Close)/Range,1)) %>% mutate(prop.RTH.C.dist.L = round((Close-L)/Range,1)) 

  ## ON/IB Comparison
  fullDates <- unique(fulldata$Date)
  final.data <- data.frame()  ## values not coming up properly with NA's   for Low, only IBL, NB for high only IBH NB
  for (i in fullDates){
            fullDailyData <- fulldata[which(fulldata[,"Date"] == i),]
            if (is.na(fullDailyData$ONH) == TRUE){  ## PROBLEM
                      next
            }
            if (is.na(fullDailyData$ONL) == TRUE){
                      next
            }
            if (invalid(fullDailyData$IBH.ONH) == TRUE){
                      next
            }
            if (invalid(fullDailyData$IBL.ONL) == TRUE){
                      next
            }
            if (fullDailyData$IBH.ONH==1 & fullDailyData$IBHBreak == 1 & fullDailyData$ONHB>0){
                      HB.comparison <- c("IBHONH") ## break ONH/IBH, IBH>ONH
                      } else if (fullDailyData$IBH.ONH==1 & fullDailyData$IBHBreak==0  & fullDailyData$ONHB>0){
                                HB.comparison <- ("ONH")## break ONH not IBH, IBH?ONH
                                } else if (fullDailyData$IBH.ONH==0 & fullDailyData$IBHBreak==1 & fullDailyData$ONHB>0){
                                          HB.comparison <- c("ONHIBH") ## break ONH/IBH, IBH<=ONH
                                          } else if (fullDailyData$IBH.ONH==0 & fullDailyData$IBHBreak==1 & fullDailyData$ONHB<=0){
                                                    HB.comparison <- c("IBH")  ## break IBH, IBH <=ONH
                                                    } else if (fullDailyData$IBHBreak==0 & fullDailyData$ONHB<=0){
                                                              HB.comparison <- c("NB")  ## no break
                                                              } else (HB.comparison <- NA)
  
  ## LOW BREAKS
            if (fullDailyData$IBL.ONL==1 & fullDailyData$IBLBreak == 1 & fullDailyData$ONLB>0){
                      LB.comparison <- c("ONLIBL") ##break both, IBL < ONL
                      } else if (fullDailyData$IBL.ONL==1 & fullDailyData$IBLBreak==0  & fullDailyData$ONLB>0){
                                LB.comparison <- ("ONL") ## break ONL, IBL < ONL
                                } else if (fullDailyData$IBL.ONL==0 & fullDailyData$IBLBreak==1 & fullDailyData$ONLB>0){
                                          LB.comparison <- c("IBL") ## break IBL, IBL > ONL
                                          } else if (fullDailyData$IBL.ONL==0 & fullDailyData$IBLBreak==1 & fullDailyData$ONLB<=0){
                                                    LB.comparison <- c("IBLONL") ## break IBL, IBL > ONL
                                                    } else if (fullDailyData$IBLBreak==0 & fullDailyData$ONLB<=0){
                                                              LB.comparison <- c("NB")  ## break neither
                                                              } else (LB.comparison <- NA)
            full <- data.frame(fullDailyData,HB.comparison,LB.comparison)
            final.data <- rbind(final.data, full)
  }
  final.data$IBHBreak <- as.factor(final.data$IBHBreak)
  final.data$IBLBreak <- as.factor(final.data$IBLBreak)
  final.data$BothBreak <- as.factor(final.data$BothBreak)
  write.csv(final.data,file=output.csv)
}
library(caret)
model.rf <- train(IBHBreak ~ p.RSI +Opening+p.rsi.comp.fast+p.rsi.comp.med+p.range.ATR.delta+p.rsi.comp.slow+GapAboveYH+
                            p.vol.delta,method="rf",ntree=200,data=final.data,trControl=trainControl(method="cv",number=3),keep.forest=TRUE)

set.seed(412)
sub <- createDataPartition(final.data$IBHBreak, p = .7,list=FALSE) 
data.train.1 <- final.data[sub,]
data.train.2 <- final.data[-sub,]
model.tree <- train(IBHBreak ~ p.RSI +Opening+p.rsi.comp.fast+p.rsi.comp.med+p.range.ATR.delta+p.rsi.comp.slow+GapAboveYH+
                              p.vol.delta,method="rpart",data=data.train.1)
library(rattle)
fancyRpartPlot(model.tree$finalModel,sub="")
tree.predict <- predict(model.tree,newdata=data.train.2)
confusionMatrix(tree.predict, data.train.2$IBHBreak)

## subset models for breaks only
HB <- filter(final.data,IBHBreak==1)


  ## Tables/percentages
final.data %>% group_by(Opening,HB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data.OAIR <- filter(final.data, Opening == "OAIR")
final.data.OAORA <- filter(final.data, Opening == "OAORA")
final.data.OAORB <- filter(final.data, Opening == "OAORB")
final.data.OAIR %>% group_by(Opening,HB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data.OAORA %>% group_by(Opening,HB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data.OAORB %>% group_by(Opening,HB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data %>% group_by(Opening,LB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data.OAIR %>% group_by(Opening,LB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data.OAORA %>% group_by(Opening,LB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))
final.data.OAORB %>% group_by(Opening,LB.comparison) %>% summarise(n =n()) %>%  mutate(freq = n / sum(n))

  table1 <- table(final.data$HB.comparison)
  table1.prop <- table1/sum(table1)
  table2 <- table(final.data$LB.comparison)
  table2.prop <- table2/sum(table2)
  
  ## table of percentages
  ##linearmodel3 <- lm(HBMFE.prop ~ p.RSI + p.vol.delta + GapAboveYH + p.IB.range.prop+range.ATR.delta + factor(Opening),data=final.data,na.action=na.omit)
  final.data.complete <- na.omit(final.data[c("HBMFE.prop", "p.RSI","p.vol.delta","GapAboveYH","p.IB.range.prop","p.range.ATR.delta","Opening")])
  linearmodel3 <- lm(HBMFE.prop ~ p.RSI + p.vol.delta + GapAboveYH + p.IB.range.prop+p.range.ATR.delta+ factor(Opening),data=final.data.complete)
  idealmodel3 <- step(linearmodel3, direction = "both")  ##Select best model
  summary(idealmodel3)
  
  final.data.complete.2 <- na.omit(final.data[c("IBHBMFE", "p.RSI","p.vol.delta","GapAboveYH","p.IB.range.prop","p.range.ATR.delta","Opening")])
  linearmodel2 <- lm(IBHBMFE ~ p.RSI + p.vol.delta + GapAboveYH + p.IB.range.prop+p.range.ATR.delta+ factor(Opening),data=final.data.complete.2)
  idealmodel2 <- step(linearmodel2, direction = "both")  ##Select best model
  summary(idealmodel2)
  ## graphs
  write.csv(RTH2,file=output.csv)
} 