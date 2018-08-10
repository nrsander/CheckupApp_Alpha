###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###            CC             ###
###         FUNCTIONS         ###
###                           ###
###   [ Redo entire thing ]   ###


##########################
###    CC Functions    ###
##########################

    ##   Return OHLCV Historical Data
    ##===================================================================================================#
    ##   ---   GetCoinHistoData        (one coin)               Returns:   VECTOR / DATA FRAME
    ##   ---   GetBagHistoData         (vector of coins)        Returns:   LIST
    ##


###  ---  Get Coin Historical Data

GetCoinHistoData <- function(InputCoin="ETH",PairCoin="BTC",timeframe="hour",pd=1000) {
  ##  Timeframe Options:    hour    minute    day
  ##  Period:               How many (timeframe)s do you want to load? 1000 days? 9000 hours? really should be common periods/sample sizes when comparing coins.
  
  ##    start using some %>%'s in this bitch ... 
  #pd=1000
  message("  * Downloading ", InputCoin, " historical price data ...")
  
  ## API call
  #url <- paste0("https://min-api.cryptocompare.com/data/histo", timeframe, "?fsym=", InputCoin, "&tsym=", PairCoin, "&allData=true", collapse = "")
  url <- paste0("https://min-api.cryptocompare.com/data/histo", timeframe, "?fsym=", InputCoin, "&tsym=", PairCoin, "&limit=", pd, collapse = "")
  coin.histo <- fromJSON(url)
  
  if (coin.histo$Response != "Success") {
    message("\tError [critical]           '",InputCoin,"' does not match any coins on CoinMarketCap.")
    message("\t",coin.histo$Response, "  ",coin.histo$Message)
    coin.data <- NA
  } else {
    coin.data <- coin.histo$Data
    coin.data$time <- as_datetime(coin.data$time)
    message("  * Download complete.")
    #message("")
    message("  * Success: Attached historical prices to your bag of ",InputCoin,".")
    message("\tPair:\t", InputCoin,"-",PairCoin)
    if (timeframe == "day") {
      message("\tPrices:\tDAILY")
    } else if (timeframe == "hour") {
      message("\tPrices:\tHOURLY")
    } else if (timeframe == "minute") {
      message("\tPrices:\tMINUTELY")
    } else {
      message("\tTimeframe:   ", pd, " [ ERROR ]")
    }
    message("\t\t<--  Rows of data requested:\t",pd)
    message("\t\t-->  Rows of data received:\t",nrow(coin.data))
    message("")
  }
  names(coin.data) <- c("time","close","high","low","open",paste0("volume_",InputCoin),"volume") # volume = denominated in PairCoin (implied)
  coin.data <- coin.data[,c(1:5,7,6)]
  coin.data
}
#zrx <- GetCoinHistoData(InputCoin="ZRX",PairCoin="BTC",timeframe="hour",pd=1000)
#head(zrx)
# View(zrx)  # Time-series (1000 hours of data) data of ZRX's OHLC prices & to/from volume




###  ---  Get Bag Historical Data

GetBagHistoData <- function(Bag,PairCoin="BTC",timeframe,pd) {
  message("Creating Bag and attaching historical prices to coins ... (0%)")
  message("")
  # pd = Retrieve the same ___x_number___ of most-recent rows for each coin
  
  #defaultbag <- c("ETH","LTC","XLM","XRP")
  #if (Bag == defaultbag) {
  #  message("{{ --- Default Bag Detected --- }}")
  #}
  
  # Create a blank list
  datalist = list()
  # For each coin in Bag...
  for (i in 1:length(Bag)) {
    inputCoin = Bag[i]
    message(inputCoin,"\t"," (Coin ",i," of ",length(Bag),")")
    message("------------------------------------------------------------------")
    if (inputCoin == "BTC") {PairCoin <- "USD"}
    chist <- GetCoinHistoData(InputCoin=inputCoin,PairCoin,timeframe,pd)
    chist <- chist[order(chist$time, decreasing = FALSE),] # = TRUE (recently changed; revert if errors.)
    print(str(chist))
    datalist[[i]] <- chist[1:pd,]
    message("")
  }
  message("Success:  Historical prices have been attached to your coins. (100%)")
  message("")
  names(datalist) <- Bag[1:length(Bag)]
  # Return list of bag data
  datalist 
}
GetBagHistoData(Bag=c("BTC","XRP"),"BTC","day",7)


