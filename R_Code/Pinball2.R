setwd("C:/Users/User/datasciencecoursera/TradingPrograms") ## set working directory
Pinball2 <- function(Data,ONHLdataCSV,outputfilename){  ### need to eliminate gaps
  data <- read.csv(Data, header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
  library(lubridate) ## for dates
  library(dplyr)
  library(gtools)  ## for testing validity
  if (invalid(data$Last)==FALSE){
    names(data)[names(data) == "Last"] <- "Close"
  }
  mav <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}  ## function for calculating moving average
  ## Calculate moving averages
  data <- mutate(data, sma.4 = mav(Close, n=4))
  data <- mutate(data,sma.5 = mav(Close, n=5))
  data <- mutate(data,sma.10 = mav(Close, n=10))
  data <- mutate(data,sma.20 = mav(Close, n=20))
  data <- mutate(data,sma.50 = mav(Close, n=50))
  ## omit rows with no 5ma
  data <- data[5:nrow(data),]
  ## calculate when bar outside 5ma
  data <- mutate(data,above.5ma = ifelse(L>sma.5,1,0))
  data <- mutate(data,below.5ma = ifelse(H<sma.5,1,0))
  ## calculate when bar crosses 4ma
  data <- mutate(data,cross.4ma = ifelse(H>sma.4|L<sma.4,1,0))
  ##  find new 5ma bars below/above
  data <- mutate(data,below.5ma.cross = ifelse(lag(below.5ma) %in% 0 & below.5ma %in% 1,1,0))
  data <- mutate(data,above.5ma.cross = ifelse(lag(above.5ma) %in% 0 & above.5ma%in% 1,1,0))
  ## SUBSET
    split, interaction???
    for i in dates, IF  above.5ma.cross = 1, subset dataframe
      for i in dates(new dataframe) IF cross.4ma = 1, entry <- Close
       break
    add date.cross to variable.names
       PROCEED
       )
exit close of next day
exit close above 4SMA
exit at percentage of 20D ATR?

  
  data <- mutate(data,close.10 = ifelse(Close>sma.10,1,0))
  data <- mutate(data,close.20 = ifelse(Close>sma.20,1,0))
  data <- mutate(data,close.50 = ifelse(Close>sma.50,1,0))
  data <- mutate(data,rep.5 = ifelse(lag(close.5)==close.5,1,0))  ## not working correctly 
}

 