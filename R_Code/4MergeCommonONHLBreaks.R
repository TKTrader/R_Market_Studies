HBreakList <- read.csv("GCHBreak.csv", stringsAsFactors = FALSE)
LBreakList <- read.csv("GCLBreak.csv", stringsAsFactors = FALSE)
HBreakList$match <- as.numeric(match(HBreakList$Date,LBreakList$Date,nomatch=0))  ## match and subset columns
HBreakList2 <- filter(HBreakList,match>0) 
write.csv(HBreakList2,file="X.csv")