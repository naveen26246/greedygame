rawdata <- read.csv("ggout.csv")

## Convert timestamp to POSIXct class
rawdata$timestamp <- as.POSIXct(rawdata$timestamp)

## Sort data by time
rawdata <- rawdata[order(rawdata$timestamp),]

getResults <- function(device){
  
  ## Get records of the particular device
   x <- rawdata[ which(rawdata$ai5 == device), ]
  
  ## Remove logs until we see the first start event 
  startIndex <- which(x$event=='ggstart')[1]
  if (is.na(startIndex)) {
    startIndex <- 0
  }
  if (startIndex > 1 ) {
    x <- x[-(1:startIndex),]
  }
  
  ## Remove records if corresponding start/stop log is missing
  reps <- sequence(rle(as.character(x$event))$lengths)
  
  indToRemove <- which(reps=="2")

  if(length(indToRemove) > 0) {
    x <- x[-c(indToRemove),]
  }
 
  ## Estimate the required metrics......
  
  if(nrow(x) > 2) {
    ## Total no. of logs
    len <- dim(x)[1]
    
    ## Remove last log if ends with start
    if(len %% 2 == 1) {
      x <- x[-(len),]
    }
    
    ## Time between consecutive events
    timeDiff <- as.numeric(diff(x$timestamp))
    
    ## Time between stop and start events
    stopStart <- timeDiff[seq(2, dim(x)[1], 2)]
    
    ## Estimate the times > 30s 
    diffSessions <- stopStart[which(stopStart > 30)]
    
    ## Find the indexes of these individual sessions
    sessionIndexes <- match(diffSessions,timeDiff)
    
    ## Find the total no. of sessions
    total_sessions <- length(sessionIndexes)
    
    ## Estimate the cumilative times for the end of each session
    cumilativeSessionTimes <- unlist(lapply(sessionIndexes,FUN= function(x) {
      sum(head(timeDiff,x-1))
    }))
    
    ## Estimate the exact times spent in each session
    actualSessionTimes <- c(cumilativeSessionTimes[1],diff(cumilativeSessionTimes))
    
    ## Neglect sessions less than 1s
    actualSessionTimes <- actualSessionTimes[which(actualSessionTimes>1)]
    
    ## Filter the valid session (> 60s)
    validSessionTimes <- actualSessionTimes[which(actualSessionTimes > 60)]
    
    ## Count the no. of valid sessions
    valid_sessions <- length(validSessionTimes)
    
    ## Estimate the average session time
    totalValidSessionTime <- sum(validSessionTimes)
    avgSessionTime <- totalValidSessionTime / valid_sessions
    
    ## Write results to file
    write(paste(device,total_sessions,valid_sessions,totalValidSessionTime,avgSessionTime,sep=",")
          ,file="results.csv",append=TRUE)
    
  }
  
}

devices <- as.character(unique(rawdata$ai5))
lapply(devices,FUN=getResults)
