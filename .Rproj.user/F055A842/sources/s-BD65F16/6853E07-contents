#  _HistoPrices.R      <---     ***
#    _TSHistoPrices.R
#      _MasterListFunctions.R
#        _MasterListTest.R
#source("Checkup_Dependencies.R")





##################
#   Functions    #
##################

##================================================================================================##
##   Download and return historical price data (OHLCV) using the CryptoCompare API.               ##
##================================================================================================##
##        Function:                    Handles input:                       Output type:          ##
##        ''''''''''''''''''''''       ''''''''''''''''''''''''''''''       '''''''''''''''''''   ##
##  -->   HistoPrices(...)             Single coin (string of ticker)       Data frame            ##
##  -->   HistoPrices.Multi(...)       Many coins (vector of tickers)       List of Data frames   ##
##================================================================================================##

















## IS THIS NECESSARY ? ----->
# It definitely isn't working right even if it is.
# It should be calling HistoPrices, not doing its own separate logic. Right?

## Multi-coin OHLCV Download
HistoPrices.Multi <- function(InputCoinSet=c("BTC","ETH","LTC","BCH"),PairCoin="BTC",timeframe="hour",pd=7*24) {

  # print
  cat("Preparing to download historical OHLCV data for the following cryptocurrencies:\n")
  for (i in 1:length(InputCoinSet)) {
    cat("(",i,")\t",InputCoinSet[[i]],"\n",sep="")
    Sys.sleep(0.4)
  }
  cat("\n\n")
  
  # make a list for collecting outputs
  List_HistoPrices = list()
  
  # for each coin in the input set ...
  for (i in 1:length(InputCoinSet)) {
    if (InputCoinSet[i] == "BTC") {
      # special case for BTC
      PairCoin <- "USD" #  **   <-- could this potentially cause errors when BTC is a member of a set of coins and PairCoin != USD???
    }
    cat("(",i,"/",length(InputCoinSet),")\t",sep="")
    coin.histoprices <- HistoPrices(InputCoin=InputCoinSet[i],PairCoin,timeframe,pd=24*7)
    coin.histoprices <- coin.histoprices[order(coin.histoprices$Time, decreasing = FALSE),] # = TRUE ? (* recently changed; revert if causes errors)
    
    List_HistoPrices[[i]] <- coin.histoprices
    #List_HistoPrices[[i]] <- coin.histoprices[1:pd,]
  }
  names(List_HistoPrices) <- InputCoinSet[1:length(InputCoinSet)]
  cat("\nSuccess: Retrieved",length(List_HistoPrices),"sets of OHLCV data.\n")
  
  # return the list of all the coins' HistoPrice dfs
  List_HistoPrices 
}
# example usage:
#XRP_and_XLM <- HistoPrices.Multi(c("BTC","XRP","XLM"),"USD","hour",24*7)
#head(XRP_and_XLM)










##################
# Error messages #
##################

Error_CCAPI.BadTicker <- function(coin.APIresponse=coin.APIresponse,InputCoin=InputCoin) {
  message("\t*** Critical Error ***")
  message(coin.APIresponse$Response,"  ",coin.APIresponse$Message)
  message("\t'",InputCoin,"' is an invalid coin ticker.")
  message("\tIf you're stuck here, refer to www.cryptocompare.com to find your coin's API-valid ticker.")
  Sys.sleep(5)
}
