
spRast_valueCoords <- function(spRaster, na_rm=FALSE){
  
  spPoints <- as.points(spRaster, na.rm=na_rm)
  
  spValues <- terra::extract(spRaster, spPoints)
  
  DIM <- dim(spValues)
  
  spRasterToPoints <- as.matrix(spValues[1:DIM[1],2:DIM[2]])
  
  spCoords <- crds(spRaster, na.rm=na_rm)
  
  list(values=spRasterToPoints, coords=spCoords)  
}

# ---

get_timeSeries_byClicking <- function(toPlot, df){
  nRow <- length(unlist(toPlot)) / 2
  
  mat_toPlot <- matrix(as.numeric(unlist(toPlot)), nrow = nRow)
  
  dX <- matrix(nrow = nrow(df))
  
  dY <- matrix(nrow = nrow(df))
  
  aproxX <- numeric(nRow)
  
  aproxY <- numeric(nRow)
  
  dX <- sapply(1:nRow, function(s) abs(df[,1] - mat_toPlot[s,1]))
  
  aproxX <- sapply(1:nRow, function(s) df[which.min(dX[,s]),1] )
  
  dY <- sapply(1:nRow, function(s) abs(df[,2] - mat_toPlot[s,2]))
  
  aproxY <- sapply(1:nRow, function(s) df[which.min(dY[,s]),2] )
  
  toExtract <- matrix(nrow = nRow, ncol = 2)
  
  toExtract[,1] <- aproxX
  toExtract[,2] <- aproxY
  #
  IND <- 1:length(df)
  xTemp <- which(df[,1] == toExtract[1,1])
  yTemp <- which(df[xTemp,2] == toExtract[1,2])
  #
  xyRow <- xTemp[yTemp] # df[xTemp[yTemp],1:2]
  
  list(coord = xyRow)
  # xyRow
}

# --- Added on May 29, 2026

getYear <- function(start=2000, end=2018, bp, freq=23){
  period <- start:end
  totalDays <- c(0, freq * 1:length(start:end))
  
  if( length(bp) == 1 ){
    year <- period[sum( totalDays - bp < 0 )]
  } else {
    year <- unlist( lapply(1:length(bp), function(s) period[sum( totalDays - bp[s] < 0 )]  ) )
  }
  
  year
}


getBreak <- function(data, start=2000, end=2018, frequency=23, bw=0.15){
  output <- NA
  breakType <- NA
  significance <- NA
  stability <- NA
  
  dataTS <- ts(data, start=c(start, 1), end=c(end, frequency),
               frequency=frequency)
  
  getBFAST <- bfast01(data=dataTS, bandwidth=bw)
  
  # if(getBFAST$breaks == 1){
  output <- getBFAST$breakpoints
  temp <- bfast01classify(getBFAST)
  breakType <- as.numeric(temp[1])
  significance <- as.numeric(temp[2])
  stability <- as.numeric(temp[3:4])
  # }
  
  list(bPs=output, type=breakType, significance=significance,
       stability=stability)
}
