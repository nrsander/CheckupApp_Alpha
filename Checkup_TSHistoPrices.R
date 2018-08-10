#  _HistoPrices.R
#    _TSHistoPrices.R    <---   ***
#      _MasterListFunctions.R
#        _MasterListTest.R
source("Checkup_HistoPrices.R")



# RUN TEST:
quick.validation.test <- TSHistoPrices.Multi()
ValidateData_TSMulti(quick.validation.test)





###################
##   Functions   ##
###################

#-  TSHistoPrices.Multi(...)
#-  TSHistoPrices(...)
#-  others ...


#  Multi-TS
#===================================

TSHistoPrices.Multi <- function(InputCoinSet=c("BTC","XRP","XLM"),PairCoin="USD",timeframe="hour",pd=7*24,OHLC="C",myTzone="UTC",validateDownload=TRUE) {  # options for timeframe_OHLCV...   "day"  |  hour"  |  "minute"

  # Run data validation at the end?
  RunDataValidationModule_TSHistoPrices.Multi <- validateDownload
  
  # Blank list for collecting output
  List_TSHistoPrices = list()
  
  # Vectorize PairCoin
  PairCoin <- rep(PairCoin,length(InputCoinSet))
  
  # For the 'price' we will consider, use the user's OHLC selection
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
    cat("\n\n\t** Warning ** \n\tInvalid OHLC selection!\n\tContinuing using Close prices ...\n\n")
  }
  
  # Download historical prices, convert to xts, and add to list
  for (i in 1:length(InputCoinSet)) {
    # call HistoPrices to download historical OHLCV data for coin i
    coinHistoPrices <- HistoPrices(InputCoinSet[i],PairCoin[i],timeframe,pd)
    coinHistoPrices <- coinHistoPrices[,c(1,priceCol,6,7)] #strip the not-selected OHLC columns
    coinHistoPrices <- coinHistoPrices[,c(1,2)] #strip volumes :()
    # convert it to a time series
    coinTS <- xts(x=coinHistoPrices[2], order.by=coinHistoPrices$Time)
    tzone(coinTS) <- myTzone # set to user's time zone
    # add it to the list
    names(coinTS) <- c("Price")
    List_TSHistoPrices[[i]] <- coinTS
  }
  
  # Set each list element (xts) 's name = the coin it represents
  names(List_TSHistoPrices) <- InputCoinSet
  
  ## Validate for Bad Data 
  ## (prints the results; can be time-consuming)
  if (RunDataValidationModule_TSHistoPrices.Multi == TRUE) {
    ValidateData_TSMulti(List_TSHistoPrices)
  }
  
  # Return the finalized list of xts objects
  List_TSHistoPrices
}





#  Single TS
#===================================
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





# Validate for Bad Data
ValidateData_TSMulti <- function(multiXTS) {
  cat("\nPerforming Error Checks ")
  for (j in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\n\n")
  Sys.sleep(0.3)
  
  failCount <- 0
  for (i in 1:length(multiXTS)) {
    Sys.sleep(0.3)
    cat("\tValidating\t",i," of ",length(multiXTS),"\t",names(multiXTS)[i],sep="")
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
  cat("\n\n._________________________________________.\n|  Validation process complete\t (100%)   |\n|-----------------------------------------'\n")
  if (failCount > 0) {
    cat("|  # Errors found:  ",failCount," of ",length(multiXTS)," failed to initialize.\n")
    Sys.sleep(1)
    Print_BadData.GotZeroPrices(names(multiXTS)[i])
  } else {
    cat("|  No errors found.\n")
  }
  cat("'-----------------------------------------'")
  Sys.sleep(0.7)
}


# Print an error report for GotZeroPrices
Print_BadData.GotZeroPrices <- function(coinName) {
  cat("|   Warning (Critical)\n|   -----------------------------------\n|   Download failed for ",coinName," (and maybe more).\n|  \tUsually solved by re-running.\n|  \tContinuing with a null-price XTS for ",coinName," ...\n\n",sep="")
}

