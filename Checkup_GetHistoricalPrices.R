
#
# Checkup_GetHistoricalPrices.R
#


##===============================================================================================================================================================================================================


##=============================================================================
## Get a list of coins' historical prices (OHLCVV columns) in several formats
##=============================================================================
## Parameters:
## -----------
##   Timeframe    -->   "day"  |   "hour"  |   "minute"
##   Period       -->   How many (timeframe)s in the past do you want?
##   ReturnType   -->   "df"   |   "xts"   |   "xts-close"
##=============================================================================

HistoOHLCV <- function(InputCoinSet=c("BTC","ETH"),PairCoin="USD",timeframe="hour",pd=2.5*31*24,myTzone="UTC",showPlot=FALSE,hurry=TRUE) {
  
  # New list to collect final output
  outputList <- list()
  
  
  ####################
  #  Validate input  #
  ####################
  
  # Print
  cat("Validating \t")

  # Make sure user typed timeframe correctly
  CC.AllowedInputs_timeframe <- c('day','hour','minute') # define the allowed API inputs
  timeframe <- tolower(timeframe) # force to lowercase
  if (substr(timeframe,nchar(timeframe),nchar(timeframe)) == "s") {timeframe <- substr(timeframe,1,nchar(timeframe)-1)} # correct if user input timeframe was mistakenly plural
  if (!timeframe %in% CC.AllowedInputs_timeframe) { 
    # after all this, if timeframe still isn't in the allowed set, then crash.
    # ERROR and crash!
  }
  
  ###         | | |          ###
  ###  ROOM FOR IMPROVEMENT  ###
  ###         v v v          ###
  # Make sure user didn't input the same input and pair coins
  for (i in 1:length(InputCoinSet)) {
    if (InputCoinSet[i] == PairCoin) {
      # Check for special exceptions
      if (InputCoinSet[i] == "BTC") {  # exception for bitcoin (right?) *
        # Warning!
        cat("\n\n(unknown warning)\n\n"); Sys.sleep(2)
        PairCoin <- "USD"
      } else {
        # Error!
        cat("\n\n(unknown error)\n\n"); Sys.sleep(2)
      }
    }
    
    # Reconcile any symbols that are different between CC and CMC
    if (InputCoinSet[i] == "MIOTA") {
      InputCoinSet[i] <- "IOT"
    }
    
    # Dismiss any coins that are not in CC database
    if  (InputCoinSet[i] == "CENNZ") {
      if (i>1) {
        cat("\n\t\tERROR!\tData unavailable for",InputCoinSet[i],"\n\t\t\tTo compensate, doubling the weight of",InputCoinSet[i-1],"\n")
        InputCoinSet[i] <- InputCoinSet[i-1]
      } else {
        cat("\n\t\tERROR!\tData unavailable for",InputCoinSet[i],"\n\t\t\tTo compensate, doubling the weight of",InputCoinSet[i+1],"\n")
        InputCoinSet[i] <- InputCoinSet[i+1]
      }
    }
    
    # Handle super-special cases
    if  (InputCoinSet[i] == "USDT") {
      if (i>1) {
        cat("\n\t\tERROR!\tIgnoring returns of Tether",InputCoinSet[i],"\n\t\t\tTo compensate, doubling the weight of",InputCoinSet[i-1],"\n")
        InputCoinSet[i] <- InputCoinSet[i-1]
      } else {
        cat("\n\t\tERROR!\tIgnoring returns of Tether",InputCoinSet[i],"\n\t\t\tTo compensate, doubling the weight of",InputCoinSet[i+1],"\n")
        InputCoinSet[i] <- InputCoinSet[i+1]
      }
    }
    
  }
  # Print
  cat("\t✓\n")
  if(!hurry){Sys.sleep(0.3)}else{Sys.sleep(0.1)}
  

  ######################
  #  Loop Thru Coins   #
  ######################
  
  cat("Downloading ...\t\n")
  # For each coin in InputCoinSet...
  for (iCoin in 1:length(InputCoinSet)) {
    
    # Print
    cat("\t(",iCoin,"/",length(InputCoinSet),")",sep="")
    cat("\t ",InputCoinSet[iCoin],"-",PairCoin,sep="")
    if (len(paste0(InputCoinSet[iCoin],PairCoin)) <= 9) {cat("\t",sep="")}
    if (len(paste0(InputCoinSet[iCoin],PairCoin)) <= 5) {cat("\t",sep="")}
    #if (nchar(InputCoinSet[iCoin])<=3) {cat("\t")}
    cat("|"); for (i in 1:8) {Sys.sleep(0.03); cat("-")}
    
    # Compose API request
    CC_URL_PREFIX.HISTO <- "https://min-api.cryptocompare.com/data/histo" # base URL for CC API
    url_APIRequest <- paste0(CC_URL_PREFIX.HISTO, timeframe, "?fsym=", InputCoinSet[iCoin], "&tsym=", PairCoin, "&limit=", pd, collapse = "")
    
    # Send API request
    coin.APIresponse <- fromJSON(url_APIRequest)
    
    # Print
    if(!hurry){for (i in 1:2) {Sys.sleep(0.1); cat("»")}} else {for (i in 1:2) {Sys.sleep(0.03); cat("»")}}
    
    ###########################
    #  Validate API Response  #
    ###########################
    
    # Check that API response is valid
    if (coin.APIresponse$Response != "Success") {
      cat("|\t X  Failure!\n")
      Sys.sleep(1)
      #Error_CCAPI.BadTicker(coin.APIresponse,InputCoinSet[iCoin])
      coin.data <- NA
      return("oopsError")
      ## We *should* crash here purposely rather than just NAing it and moving on...
    } else {
      coin.data <- coin.APIresponse$Data
      cat("|\t ✓  Data")
    }
    # Check that there are no NAs or Infs
    #   We *should* be checking here at the data-download level so that we can immediately retry ... but if there are, how the frick are we going to even handle random NA gaps?
    
    #############################
    #    Clean Response Data    #
    #############################
    
    # Rearrange columns 
    coin.data <- coin.data[,c(1,5,3,4,2,7,6)]
    
    # Convert date-time string into DateTime object
    coin.data$time <- as_datetime(coin.data$time)
    
    # Name the columns (quantmod-compatible names ✓)
    names(coin.data) <- c("Time","Open","High","Low","Close",paste0("Vol_",InputCoinSet[iCoin]),paste0("Vol_",PairCoin)) # volume = denominated in PairCoin (implied)
    for (n in 2:length(coin.data)) {
      names(coin.data)[n] <- paste0(names(coin.data)[n],".",InputCoinSet[iCoin]) # suffix ".[ticker]" to col names (except datetime column of course)
    }
    
    # Create the xts
    coin.data.xts <- xts(x=coin.data[,2:ncol(coin.data)], order.by=coin.data[,1])
    tzone(coin.data.xts) <- myTzone # user's tzone, if provided (default = UTC)
    
    # From the xts, make a 'simple price' xts vector (only Close)
    coin.data.xts_Close <- coin.data.xts[,4]
    
    # From the xts, make a quantmod object (full OHLCvV feature set)
    Qm_ColNameRoots <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    coin.data.qm <- as.quantmod.OHLC(coin.data.xts, col.names=Qm_ColNameRoots, name=InputCoinSet[iCoin])
    
    # Plot Qm chart briefly
    if (showPlot == TRUE) {
      iPlotTitle <- paste0(InputCoinSet[iCoin],"-",PairCoin," Price",sep="")
      chartSeries(coin.data.qm, type = "bars", theme = chartTheme("black"),name=iPlotTitle)
      cat("\t✓ Plot")
      Sys.sleep(0.05)
    }
    cat("\n")

    ####################
    #  Create subList  #
    ####################
    
    # Add our various transformations of this coin's OHLCvV  data to the subList
    subList <- list()
    subList[[1]] <- coin.data.xts_Close # [subList-1]  xts-close (xts vector of Close; 'just the price')
    subList[[2]] <- coin.data.xts # [subList-2]  xts (full OHLCvV)
    subList[[3]] <- coin.data # [subList-3]  df (full OHLCvV)
    subList[[4]] <- coin.data.qm # [subList-4]  quantmod object (full OHLCvV)
    
    # Name subList elements
    subList.titles <- c("xts_vector","xts","df","qm")
    for (sL.t in 1:length(subList.titles)) {names(subList)[sL.t] <- paste0(InputCoinSet[iCoin],".",subList.titles[sL.t],sep="")} # this naming may be redundant... (***)
    names(subList) <- subList.titles # THIS JUST UNDOES THAT^... (***)
    
    ##  subList elements:
    ##  -----------------
    ##     1:4  =  Price (simple xts vector 'close price', OHLCvV xts, OHLCvV df, quantmod object)
    ##       5  =  Periodic returns (***)
    
    # Add iCoin's subList (multiple convenient formats of iCoin's OHLCvV data) to the outputList
    outputList[[iCoin]] <- subList
  }
  cat("\n")
  
  ###########################
  #  Return list of OHLCVs  #
  ###########################
  
  # Name outputList elements
  names(outputList) <- InputCoinSet
  
  # Return the coins' historical prices, in a list containing several convenient OHLCvV formats
  outputList
  
}



##===============================================================================================================================================================================================================



# Make 'Combined' xts of all the coins' histo Closes from our custom List
GetHistoClosesCombined <- function(myListOfOHLCVs=HistoOHLCV()) {
  # Constant
  SUBLIST_INDEX_CLOSE <- 1
  # List of coins' histo Close xts vectors
  close_prices_list <- list()
  
  # Extract mhCoin's histo Close xts vector; put it into our list of all coins' histo Closes
  for (nCoin in 1:length(myListOfOHLCVs)) {
    cat("\rExtracting Close Prices ... \t",round(nCoin/length(myListOfOHLCVs),0)*100,"%",sep="")
    close_prices_list[[nCoin]] <- myListOfOHLCVs[[nCoin]][[SUBLIST_INDEX_CLOSE]]
    Sys.sleep(0.4)
  }
  cat("\n");Sys.sleep(0.1)
  names(close_prices_list) <- names(myListOfOHLCVs)
  
  # Merge list of histo Closes into one all-coin xts of histo Closes
  close_prices_xts <- do.call(merge, close_prices_list)
  # Return combined xts
  close_prices_xts
}



##===============================================================================================================================================================================================================



#====================,
#     Test Script    |
#===================='

# Make an example Price Bundle
###example_PriceBundle <- HistoOHLCV(InputCoinSet=c("ETH","ETC","NEO","GAS"),PairCoin="USD",timeframe="hour",pd=2*31*24,myTzone="UTC",showPlot=TRUE)

### Plot each subList element briefly (of coin #1 only)
##subList_titles <- names(example_PriceBundle[[1]])
##for (sLt in 1:length(subList_titles)) {print(tail(example_PriceBundle[[1]][[subList_titles[sLt]]]));Sys.sleep(2)}
#
### Test run GetHistoPricesCombined(...)
##comboClose <- GetHistoPricesCombined(example_PriceBundle)
##print(tail(comboClose))
#
#


##===============================================================================================================================================================================================================


# API response validation
#    Check for errors in API response, in xts conversion, in whatever
ValidateData_TSMulti <- function(multiXTS,hurry=FALSE) {
  
  # Print
  cat("\nIntegrity check ")
  for (j in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\n\n")
  if(!hurry){Sys.sleep(0.5)}else{Sys.sleep(0.1)}
  
  failCount <- 0
  for (i in 1:length(multiXTS)) {
    if(!hurry){Sys.sleep(0.3)}else{Sys.sleep(0.1)}
    cat("\tValidating    ",i," of ",length(multiXTS),"\t",names(multiXTS)[i],sep="")
    if (nchar(names(multiXTS)[i]) <= 3) {cat("\t",sep="")}
    cat("  [")
    if(!hurry){for (g in 1:10) {Sys.sleep(0.1); cat("»")}}else{cat("»»»»»»»»»»")}
    cat("|")
    if(!hurry){Sys.sleep(0.3)}
    cat("\t")
    
    # review contents {debug}
    #for (f in 1:length(multiXTS)) {print(multiXTS[[Q]][1]);Sys.sleep(1)}
    
    # check for GotZeroPrices error (failed download or conversion)
    if (sum(multiXTS[[i]][1:5]) == 0) {
      cat("✕  Bad\t\t",names(multiXTS)[i],"\n",sep="")
      if(!hurry){Sys.sleep(1)}else{Sys.sleep(0.1)}
      failCount <- failCount + 1
    }
    # perform any other error checks here
    # else if (...) {...}
    else {
      cat("✓  Validated\n",sep="")
      if(!hurry){Sys.sleep(1)}else{Sys.sleep(0.1)}
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
  if(!hurry){Sys.sleep(0.7)}else{Sys.sleep(0.1)}
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



##===============================================================================================================================================================================================================


# anything else
# ...




