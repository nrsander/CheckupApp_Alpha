#  _HistoPrices.R
#    _TSHistoPrices.R    <---   ***
#      _MasterListFunctions.R
#        _MasterListTest.R
source("Checkup_HistoPrices.R")



# AMAZING !!!

# Run Test:
full.OHLCV.xts <- List_OHLCV.xts()
# See Results:
for (ie in 1:length(full.OHLCV.xts)) {
  print(head(tushy[[ie]]))
  Sys.sleep(1)
}




###################
##   Functions   ##
###################

#-  List_OHLCV.xts
#-  validation functions ...



#  List_OHLCV.xts
#================================================

List_OHLCV.xts <- function(InputCoinSet=c("BTC","ETH","XMR","ZEC"),PairCoin="USD",timeframe="day",pd=90,myTzone="EST",validateDownload=TRUE) {  # options for timeframe_OHLCV...   "day"  |  hour"  |  "minute"

  # Print
  cat("\nScraping OHLCV data  ","(last ",pd," ",timeframe,"s) ",sep="")
  for (ij in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\n\n")
  Sys.sleep(0.3)
  
  # Vectorize PairCoin
  PairCoin <- rep(PairCoin,length(InputCoinSet))
  
  # Blank list for collecting output
  List_TSHistoPrices = list()
  
  # Download OHLCV data via HistoPrices() 
  for (i in 1:length(InputCoinSet)) {
    List_TSHistoPrices[[i]] <- HistoPrices(InputCoinSet[i],PairCoin[i],timeframe,pd)
    names(List_TSHistoPrices)[i] <- InputCoinSet[i]
  }
  
  # Print
  cat("\nConstructing time-series ")
  for (ji in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\tAll  ")
  Sys.sleep(0.3)

  
  # Make xts and add to list
  for (i in 1:length(InputCoinSet)) {
    # make xts
    coinXTS <- xts(x=List_TSHistoPrices[[i]][,2:ncol(List_TSHistoPrices[[i]])], order.by=List_TSHistoPrices[[i]]$Time)
    # change to user's timezone if provided; default = UTC
    tzone(coinXTS) <- myTzone
    # give each column a quantmod-compatible OHLC name
    for (ijh in 1:length(names(coinXTS))) {
      names(coinXTS)[ijh] <- paste0(names(List_TSHistoPrices)[i],".",names(coinXTS)[ijh],sep="")
    }
    # add coin i's price xts to the list
    List_TSHistoPrices[[i]] <- coinXTS
  }
  
  # Name of each element in list (aka each xts) = the coin it represents
  names(List_TSHistoPrices) <- InputCoinSet
  
  # Print (done w/ mass-constructing time series)
  Sys.sleep(0.4)
  cat("\t\t\t✓  Done\n",sep="")
  
  # Wanna perform download validation?
  if (validateDownload == TRUE) {
    # performing validation = doubles the time needed to run
    ValidateData_TSMulti(List_TSHistoPrices) # prints results 
  }
  
  # Print
  cat("\nDone.\n")
  
  # Return finalized list of xts'es
  List_TSHistoPrices
}



#  Validation
#================================================

# Run validation process
#    Check for errors in API response, in xts conversion, in whatever
ValidateData_TSMulti <- function(multiXTS) {
  
  # Print
  cat("\nIntegrity check ")
  for (j in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\n\n")
  Sys.sleep(0.3)
  
  failCount <- 0
  for (i in 1:length(multiXTS)) {
    Sys.sleep(0.3)
    cat("\tValidating    ",i," of ",length(multiXTS),"\t",names(multiXTS)[i],sep="")
    if (nchar(names(multiXTS)[i]) <= 3) {cat("\t",sep="")}
    cat("  [")
    for (g in 1:10) {Sys.sleep(0.1); cat("»")}
    cat("]")
    Sys.sleep(0.3)
    cat("\t")
    
    # review contents {debug}
    #for (f in 1:length(multiXTS)) {print(multiXTS[[Q]][1]);Sys.sleep(1)}
    
    # check for GotZeroPrices error (failed download or conversion)
    if (sum(multiXTS[[i]][1:5]) == 0) {
      cat("✕  Bad\t\t",names(multiXTS)[i],"\n",sep="")
      Sys.sleep(1)
      failCount <- failCount + 1
    }
    # perform any other error checks here
    # else if (...) {...}
    else {
      cat("✓  Validated\n",sep="")
      Sys.sleep(1)
    }
  }
  
  # summarize errors
  cat("\n\t")
  cat(",_________________________________________,\n\t")
  cat("|  Validation process complete\t (100%)   |\n\t")
  cat("|-----------------------------------------|\n\t")
  if (failCount > 0) {
    cat("|  # Errors found:  ",failCount," of ",length(multiXTS)," failed to initialize.\n\t")
    Sys.sleep(1)
    Print_BadData.GotZeroPrices(failCount)
  } else {
    cat("|  No errors found.                       |\n\t")
  }
  cat("'-----------------------------------------'\n")
  Sys.sleep(0.7)
}

# Print error reports
#    GotZeroPrices
Print_BadData.GotZeroPrices <- function(numFails) {
  cat("|         Warning (Critical)\n|   -----------------------------------\n|   Download failed for ",numFails," coin",sep="")
  if (numFails > 1) {cat("s",sep="")}
  cat(".\n|  \tOften solved by re-running.\n|  \tContinuing with a null-price XTS for ",numFails," coin",sep="")
  if (numFails > 1) {cat("s",sep="")}
  cat(" ...\n",sep="")
}








#  Single TS
#================================================

TSHistoPrices <- function(InputCoin="BTC",PairCoin="USD",timeframe="hour",pd=7*24,OHLC="C",myTzone="UTC") {  # options for timeframe_OHLCV...   "day"  |  hour"  |  "minute"
  
  # Download historical prices
  coinHistoPrices <- HistoPrices(InputCoin,PairCoin,timeframe,pd)
  
  # For our 'price' we will consider, use the user's OHLC selection
  priceCol <- 5
  if (OHLC == "O") {
    priceCol <- 2
  } else if (OHLC == "H") {
    priceCol <- 3
  } else if (OHLC == "L") {
    priceCol <- 4
  } else if (OHLC == "C") {
    priceCol <- 5
  } else {
    priceCol <- 5
  }
  
  # convert to a time series
  coinTS = xts(x=coinHistoPrices[priceCol], order.by=coinHistoPrices$Time)
  tzone(coinTS) <- myTzone # set to user's time zone
  # return the time series
  coinTS
}


