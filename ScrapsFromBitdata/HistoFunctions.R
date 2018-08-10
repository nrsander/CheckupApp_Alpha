########################################
###           HistoFunctions         ###
########################################


#######################
###   Dependencies  ###
#######################

library(coinmarketcapr) # neat
library(rvest) # read web data
library(jsonlite) # downloading CC API and other internet stuff someday
library(dplyr) # getting our hands dirty with Big Data
library(stringr) # better text
library(forecast) # forecasting
library(plotrix) # plotting utility
library(tidyr) # clean data is better data
library(ggplot2) # visualization
library(lubridate) # date times


#######################
###    Functions    ###
#######################



##############################
###        Function        ###
### ---------------------- ###
###    GET COIN HISTORY    ###
##############################
GetCoinHistory <- function(InputCoin="ETH",PairCoin="BTC",timeframe="hour",pd=1000) {
  ##  Timeframe Options:    hour    minute    day
  ##  Period:               How many (timeframe)s do you want to load? 1000 days? 9000 hours? really should be common periods/sample sizes when comparing coins.
  
  ##    start using some %>%'s in this bitch ... 
  pd=1000
  message("Downloading market data for ", InputCoin, " ...")
  
  ## API call
  url <- paste0("https://min-api.cryptocompare.com/data/histo", timeframe, "?fsym=", InputCoin, "&tsym=", PairCoin, "&allData=true", collapse = "")
  coin.histo <- fromJSON(url)
  
  if (coin.histo$Response != "Success") {
    message("         ERROR [Critical]:          '",InputCoin,"' does not match any coins in the data repository.")
    message("         ",coin.histo$Response, "  ",coin.histo$Message)
    coin.data <- NA
  } else {
    # coin.data
    coin.data <- coin.histo$Data
    #str(coin.data)
    coin.data$time <- as_datetime(coin.data$time)
    # Order by date, descending
    # ...............
    head(coin.data)
    message("   Success: ", InputCoin, " market data is now available.")
    message("        Pair:        ", PairCoin)
    if (timeframe == "day") {
      message("        Timeframe:   ", "Daily prices (close)")
    } else if (timeframe == "hour") {
      message("        Timeframe:   ", "Hourly prices (close)")
    } else if (timeframe == "minute") {
      message("        Timeframe:   ", "Minutely prices (close)")
    } else {
      message("        Timeframe:   ", "[ERROR]")
    }
  }
  coin.data
}

zrx <- GetCoinHistory(InputCoin="ZRX",PairCoin="BTC",timeframe="hour",pd=1000)



##############################
###        Function        ###
### ---------------------- ###
###     BUILD COIN DATA    ###
##############################
BuildCoinData <- function(Bag=c("ETH","LTC","XLM","XRP"),PairCoin="BTC",pd=1000) {
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
    message("Processing coin ",inputCoin," ...")
    if (inputCoin == "BTC") {PairCoin <- "USD"}
    chist <- GetCoinHistory(InputCoin=inputCoin,PairCoin,timeframe="hour")
    chist <- chist[order(chist$time, decreasing = TRUE),]
    datalist[[i]] <- chist[1:pd,]
  }
  message("Bag created!   (# of coins: ", length(datalist),")")
  names(datalist) <- Bag[1:length(Bag)] 
  
  # Return list of bag data
  datalist 
}


##############################
###        Function        ###
### ---------------------- ###
### GET PRICE AT TIMESTAMP ###
##############################
GetPriceAtTimestamp <- function(InputCoin="BTC",PairCoin="USD",TimestampForPrice=1516503600) {
  url <- paste0("https://min-api.cryptocompare.com/data/pricehistorical", "?fsym=", InputCoin, "&tsyms=", PairCoin, "&ts=", TimestampForPrice, collapse = "")
  jdata <- fromJSON(url)
  jprice <- jdata[[1]][[1]]
  jprice
}
price <- GetPriceAtTimestamp(InputCoin="ETH",PairCoin="USD",TimestampForPrice = 1516503600)
price


##############################
###        Function        ###
### ---------------------- ###
###  GET BTC AT TIMESTAMP  ###
###           &            ###
###  GET ETH AT TIMESTAMP  ###
##############################
GetBTCAtTimestamp <- function(TimestampForPrice=1516503600) { # random timestamp is default
  if (TimestampForPrice==1516503600) {
    #message("WARNING: I detect that the historical Bitcoin prices being downloaded may not be what you requested. Did you specify the timestamp properly?")
    #message("Continuing anyway...")
  }
  InputCoin="BTC"
  PairCoin="USD"
  btcurl <- paste0("https://min-api.cryptocompare.com/data/pricehistorical", "?fsym=", InputCoin, "&tsyms=", PairCoin, "&ts=", TimestampForPrice, collapse = "")
  bdata <- fromJSON(btcurl)
  bprice <- bdata[[1]][[1]]
  bprice
}
GetBTCAtTimestamp(1506753833)

GetETHAtTimestamp <- function(TimestampForPrice=1516503600) { # random timestamp is default
  if (TimestampForPrice==1516503600) {
    #message("WARNING: I detect that the historical Ethereum prices being downloaded may not be what you requested. Did you specify the timestamp properly?")
    #message("Continuing anyway...")
  }
  InputCoin="ETH"
  PairCoin="USD"
  ethurl <- paste0("https://min-api.cryptocompare.com/data/pricehistorical", "?fsym=", InputCoin, "&tsyms=", PairCoin, "&ts=", TimestampForPrice, collapse = "")
  edata <- fromJSON(ethurl)
  eprice <- edata[[1]][[1]]
  eprice
}
GetETHAtTimestamp(1516503000)


##############################
###        Function        ###
### ---------------------- ###
###  GET BTC AT TIMESTAMP  ###
##############################
GetETHAtTimestamp <- function(TimestampForPrice=1516503600) { # random timestamp is default
  if (TimestampForPrice==1516503600) {
    #message("WARNING: I detect that the historical Ethereum prices being downloaded may not be what you requested. Did you specify the timestamp properly?")
    #message("Continuing anyway...")
  }
  InputCoin="ETH"
  PairCoin="USD"
  ethurl <- paste0("https://min-api.cryptocompare.com/data/pricehistorical", "?fsym=", InputCoin, "&tsyms=", PairCoin, "&ts=", TimestampForPrice, collapse = "")
  edata <- fromJSON(ethurl)
  eprice <- edata[[1]][[1]]
  eprice
}
GetETHAtTimestamp(1516503000)