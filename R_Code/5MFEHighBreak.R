HMFE <- function(HBCSV,outputfilename){
      library(dplyr)
      data <- read.csv(HBCSV, stringsAsFactors = FALSE)
      dataMFE <- mutate(data,MFE = H - ONH,MFE.close = Close - ONH)
      write.csv(dataMFE,file=outputfilename) 
}