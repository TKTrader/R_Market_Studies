LMFE <- function(LBCSV,outputfilename){
      library(dplyr)
      data <- read.csv(LBCSV, stringsAsFactors = FALSE)
      dataMFE <- mutate(data,MFE = ONL - L,MFE.close = ONL - Close)
      write.csv(dataMFE,file=outputfilename) 
}

mytable <- table(dataMFE$MFE,dataMFE$ONRange)  ##work into graph?