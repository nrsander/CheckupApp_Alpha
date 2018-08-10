###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###          CCHist           ###
###         FUNCTIONS         ###



#----------------------------------------------------------,
#     ,,,,,,,,,,,,,                                        |
#-----| FUNCTIONS |--\----------------------\--------------'
#     ''''''''''''     \                      \
#                        \                      \
#     GET PRICES ON DATE  |   c(YYYY,MM,DD),     |  Returns a numeric *vector* of prices
#                         |   c("XLM","BAT",...) |
#                         |   "USD"  |  "BTC"    |
#                        /   0/1 print $ msgs   /
#                    ----\----------------------\
#      GET PRICE ON DATE  |   "BTC", "USD",      |
#                         |   "YYYY","MM","DD"   |
#------------------------/----------------------/
#                        \                      \
#  GET PRICE ON DATETIME  |   "ETH", "USD",      |
#                         |   "YYYY","MM","DD"   |
#                         |   "HH", "MM", "SS",  |
#                         |   "Tz"               |
#------------------------/----------------------/


#-----------------------|-----------------------\
#                        \                       |
#       MAKE TIME SERIES  |   InputCoin = "BTC", |
#                         |   PairCoin = "USD",  |
#          * * * * *      |   Days = 365,        |
#                         |   OHLC = "[O/H/L/C]" |
#------------------------/----------------------/


#-----------------------------------------------\
#                       '''''''\                 |
# GET UNIX TIMESTAMP_DATETIME   |   Date-Time    |
#                    ----/'''''' ---------------/
#                        \                      \
# GET UNIX TIMESTAMP_DATE |   *Only* Date        |
#                    ----/ ---------------------/
#                        \                      \
#  DECODE UNIX TIMESTAMP  |   UNIX --> numeric   |
#-------------------------'----------------------'


#,-----------------------------------------------,
#    Get Histo Price of Coin at Given Timestamp:  |
#>-----------------------------------------------<;
#    ---->   GetPriceAtTimestamp     (any coin)   |
#        -->   GetBTCAtTimestamp     (BTC)        |
#        -->   GetETHAtTimestamp     (ETH)        |
#'______________________________________________.-'


GetPricesOnDate <- function(dateVec=c(2017,10,03),tckrs=myBag,denom=PairCoin,showMsgs=1) {
  message("Downloading historical prices ...")
  ret <- c()
  
  # Make strings for CC API calls
  YYYY <- as.character(dateVec[1])
  MM <- as.character(dateVec[2])
  DD <- as.character(dateVec[3])
  
  # Data validation
  
  #!turn input into uppercase characters!
  if (nchar(YYYY) == 2) {
    YYYY <- paste0("20",YYYY)
    #YYYY <- paste("20",YYYY)
  }
  if (nchar(MM) == 1) {
    MM <- paste0("0",MM)
  }
  if (nchar(DD) == 1) {
    DD <- paste0("0",DD)
  }
  if (denom == "$") {
    denom <- "USD"
  }
  symb <- ""
  symb2 <- ""
  roundTo <- 8
  if (denom == "USD") {
    symb <- "$"
    symb2 <- ""
    roundTo <- 2
  } 
  if (denom == "BTC") {
    symb <- ""
    symb2 <- " BTC"
    roundTo <- 8
  }
  message("")
  for (i in 1:length(tckrs)) {
    myPriceOnDate <- GetPriceOnDate(InputCoin=tckrs[i],denom=denom,str.Year=YYYY,str.Month=MM,str.Day=DD) # This is a DF (full-fledged response) NOT a numeric
    myPriceOnDate$Price <- round(myPriceOnDate$Price,roundTo)
    if (showMsgs != 0) message(tckrs[i],"\t",symb,myPriceOnDate$Price,symb2, "\t (", round(i/length(tckrs),2)*100,"%)")
    ret[i] <- myPriceOnDate$Price
  }
  message("[Download complete]")
  message("")
  
  ret
}
ret <- GetPricesOnDate()

#-
GetPriceOnDate <- function(InputCoin="BTC", denom="USD", str.Year="2017",str.Month="10",str.Day="03") {
  # default = bitcoin price on my birthday
  unix_tmstmp <- GetUNIXTimestamp_Date(str.Year,str.Month,str.Day) #Date
  df <- data.frame(DecodeUnixTimestamp(unix_tmstmp),InputCoin,GetPriceAtTimestamp(InputCoin,denom,as.numeric(unix_tmstmp)),denom) ## WRONG TIME ZONE!!! EDT
  names(df) <- c("Date","Coin","Price","Denom")
  df
}


#-
GetPriceOnDateTime <- function(InputCoin="BTC", denom="USD", str.Year="2017",str.Month="10",str.Day="03",str.Hour="12",str.Minute="34",str.Second="00",str.TimeZone="EST") {
  # default = bitcoin price on my birthday at 12:34:00 PM
  unix_tmstmp <- GetUNIXTimestamp_DateTime(str.Year,str.Month,str.Day,str.Hour,str.Minute,str.Second,str.TimeZone) #DateTime
  df <- data.frame(DecodeUnixTimestamp(unix_tmstmp), InputCoin, GetPriceAtTimestamp(InputCoin, denom, as.numeric(unix_tmstmp)), denom)
  names(df) <- c("Date","Coin","Price","Denom")
  df
}




#-  ---  Get Price [of any coin] At Timestamp
GetPriceAtTimestamp <- function(InputCoin="BTC",PairCoin="USD",TimestampForPrice=1516503600) {
  url <- paste0("https://min-api.cryptocompare.com/data/pricehistorical", "?fsym=", InputCoin, "&tsyms=", PairCoin, "&ts=", TimestampForPrice, collapse = "") # works with a single PairCoin only, despite what their API says
  jdata <- fromJSON(url)
  jprice <- jdata[[1]][[1]]
  jprice
}
#price <- GetPriceAtTimestamp(InputCoin="ETH",PairCoin="USD",TimestampForPrice = 1516503600)
#price


#-  ---  Get BTC [Price] At Timestamp
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
#GetBTCAtTimestamp(1506753833)


#-  ---  Get ETH [Price] At Timestamp
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
#GetETHAtTimestamp(1516503000)










#- Make Time Series
MakeTimeSeries <- function(InputCoin="BTC",PairCoin="USD",Days=365,OHLC="C") {
  
  df <- GetCoinHistoData(InputCoin,PairCoin,"day",Days)
  
  priceCol <- 2
  if (OHLC == "C") {
    priceCol <- 2
  } else if (OHLC == "H") {
    priceCol <- 3
  } else if (OHLC == "L") {
    priceCol <- 4
  } else if (OHLC == "O") {
    priceCol <- 5
  } else {
    priceCol <- 2
  }
  
  #dfts = ts(df[, c(priceCol)], start=c(2017,05,03),frequency=365) # Make the all-important ts object
  todayDate <- today(tzone="EST")
  todayDate
  tdate <- as.numeric(str_split(today(tzone="EST"),"-")[[1]])
  #dfts = ts(df[, c(priceCol)], start=tdate,frequency=Days) # Make the all-important ts object
  dfts = ts(df[, c(priceCol)], start=c(2018,12),frequency=Days)
  
  plot(dfts,main=InputCoin,ylab=paste0(InputCoin," Price (",PairCoin,")"),xlab=paste0("Date (Last ",Days," days)"))
  dfts
}







# GET UNIX TIMESTAMP      Date-Time
# Get DateTime numeric UNIX timestamp from string-date&time inputs      (Y,M,D,H,M,S,TZ)
GetUNIXTimestamp_DateTime <- function(str.Year="2017",str.Month="10",str.Day="03",str.Hour="12",str.Minute="00",str.Second="00",str.TimeZone="EST") {
  uts_dt <- 1234567890
  str.Date <- paste(str.Year,str.Month,str.Day,sep="-")
  str.Time <- paste(str.Hour,str.Minute,str.Second,sep=":")
  str.DateTime <- paste(str.Date,str.Time,str.TimeZone,sep=" ")
  uts_dt <- as.numeric(as.POSIXct(str.DateTime))
  uts_dt # return timestamp
}


# GET UNIX TIMESTAMP      DATE ONLY  (12:00 AM of selected date)
# Get Date numeric UNIX timestamp from string date inputs      (Y,M,D)
GetUNIXTimestamp_Date <- function(str.Year="2017",str.Month="10",str.Day="03") {
  uts_d <- 1234567890
  str.Date <- paste(str.Year,str.Month,str.Day,sep="-")
  uts_d <- as.numeric(as.POSIXct(str.Date, format="%Y-%m-%d"))
  uts_d # return timestamp
}


# DECODE UNIX TIMESTAMP   DATE-TIME
# Get Date string from UNIX timestamp (numeric) input
DecodeUnixTimestamp <- function(unix_tmstmp) {
  s <- anytime(unix_tmstmp)
  s
}
