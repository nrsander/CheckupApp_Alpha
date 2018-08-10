
##                 ##
##  PortfolioStat  ##
##                 ##

# Coins
coinTickers <- c("BTC","ETH","BCH","XMR","ZEC","ETC","DOGE","DGB","ZRX")

# Lags
k_Lags <- 1
k_Rolling <- 60

# Date Range
trimDateRange <- FALSE
start_time <- "2018-03-19 12:00:00" # these only apply if TRUE
end_time <- "2018-06-03 12:00:00"


#########################################################################################


# Generate OHLCV bundle
coins <- HistoOHLCV(InputCoinSet=coinTickers,PairCoin="USD",timeframe="minute",pd=1.3*24*60,myTzone="UTC",showPlot=TRUE)

# Make a list containing all the coins' quantmod dfs
qms <- list()
for (i in 1:length(coins)) {
  print(tail(coins[[i]][[4]]))
  Sys.sleep(0.3)
  qms[[i]] <- coins[[i]][[4]]
}
names(qms) <- InputCoinSet

# Combine close prices
coins_Close <- GetHistoClosesCombined(coins)

# (optional) Trim to custom time range
if (trimDateRange == TRUE) {coins_Close <- coins_Close[paste0(start_time,"/",end_time)]}


#########################################################################################


# Portfolio-statistic functions
coins_Returns <- Calculate_Returns(coins_Close,k_Lags=k_Lags,TRUE) #  X
coins_LogReturns <- Calculate_LogReturns(coins_Close,k_Lags=k_Lags,TRUE) #  X
coins_RollingReturns <- Calculate_RollingReturns(coins_Close,k_Lags=k_Lags,k_Rolling=k_Rolling,TRUE) # Useful graph!
# Portfolio-statistic plots
Calculate_BetaCorrelations(coins_Close) # (table) Useful!
Calculate_BetaRolling(coins_Close,selectCoins=c(2,3,4,1),k_Lags=k_Lags,k_Rolling=k_Rolling,return=FALSE) # Useful!   (Order of coins matters)
Calculate_BetaRollingCorrelations(coins_Close,selectCoins=c(1,2,3,4),k_Rolling=k_Rolling,return=FALSE) # Useful!   (Order doesn't matter, but + coins increases # of results exponentially)


#########################################################################################

#
# Split
# Into
# Train & Test
# Sets
#

# Threshold
trainPct <- 0.75

# Split data
trainSet <- coins_Returns[c(1:(trainPct*nrow(coins_Returns))),]
testSet <- coins_Returns[c(((trainPct*nrow(coins_Returns))+1):nrow(coins_Returns)),]
trainCount <- nrow(trainSet)
testCount <- nrow(testSet)

# Catch any errors resulting from train/test division
if(trainCount+testCount-nrow(coins_Returns) != 0) {
  cat("[ERROR] Division into train and test sets failed\n\t")
  if (trainCount+testCount-nrow(coins_Returns) < 0) {
    cat("Train and test are missing",-(trainCount+testCount-nrow(coins_Returns)),"elements from the full data set.\n\n")
  } else {
    cat("There are",trainCount+testCount-nrow(coins_Returns),"elements with overlapping indices.\n\n")
  }
}


#trainSet
#testSet


#########################################################################################

# 
# Strategy
# Comparison
# Simulation
# 

xCoins <- coins
xQms <- qms
xReturns <- coins_Returns
#xData <- data




# Strategies
# -----------------
# (1) Random
# (2) Buy and Hold
# (3) Momentum
# (4) UPD
# (5) RSI
# (6) (other TA?)


coinIndex <- 6

### SMAs
Strategy7 <- function(data=xQms[[coinIndex]],SMA_intervals=c(30,90,200)) {
  cat("\nStrategy 7")
  # Data
  price <- Cl(data)
  mySMAs <- list(paste0("addTA(SMA(price,",SMA_intervals[1],"), col=2, on=1)"), paste0("addTA(SMA(price,",SMA_intervals[2],"), col=5, on=1)"),paste0("addTA(SMA(price,",SMA_intervals[3],"), col=6, on=1)"))
  # Chart
  chartSeries(price, TA=mySMAs) # Overlay SMAs on chart
  # Print
  Sys.sleep(0.3)
  cat("\t100%\n")
}
Strategy7()


# MACD
Strategy8 <- function(data=xQms[[coinIndex]]) {
  cat("\nStrategy 8\n")
  
  # Get Close price data
  price <- Cl(data)
  # Chart MACD
  macd = MACD(price, nFast=10,nSlow=30,nSig=9,maType=SMA,percent=FALSE)
  chartSeries(price, TA="addMACD()")
  # Generate signal
  signal <- Lag(ifelse(macd$signal > macd$macd, 1, -1)) # -1 = short
  signal
  # Calculate returns
  returns <- ROC(price)*signal
  returns
  ## Trim date range
  ##returns <- returns['2018-06-02/2018-07-02']
  
  # Combine relevant info into a new xts
  cat("Generating trading signals ...\n");Sys.sleep(0.3)
  SignalReturns <- xts(x=data.frame(macd$macd, signal, returns),order.by=index(returns))
  names(SignalReturns) <- c("macd","signal","returns")
  # Remove all rows that contain any NA values
  zrows <- nrow(SignalReturns)
  SignalReturns <- SignalReturns[complete.cases(SignalReturns),]
  cat("Removed",(zrows-nrow(SignalReturns)),"of",zrows,"rows of observations (NA values)\n")
  
  # Calculate and plot cumulative returns
  cum_returns <- exp(cumsum(SignalReturns$returns))
  print(plot(cum_returns))
  
  # Analyze performance
  drawdowns <- table.Drawdowns(SignalReturns$returns, top=10)
  cat("Drawdowns:\n")
  print(drawdowns)
  #print(table.DownsideRisk(returns))
  #charts.PerformanceSummary(SignalReturns$returns)
  
  # Print
  Sys.sleep(0.3)
  cat("100%\n\nReturn:\t",(round(cum_returns[length(cum_returns)],4)*100)-100,"%\n\n",sep="")
  
  # Return
  rets <- cbind(SignalReturns,cum_returns)
  names(rets) <- c("macd","signal","returns","cum_returns")
  rets
  
}
sigRets <- Strategy8()
tail(sigRets)


### Random
Strategy1 <- function(data=trainData) {
  
  
}

### Buy and Hold
Strategy2 <- function(data=trainData) {
  
  
}

### Momentum
Strategy3 <- function(data=trainData) {
  
  
}

### UPD
Strategy4 <- function(data=trainData) {
  
  
}

### RSI
Strategy5 <- function(data=trainData) {
  
  
}

### (other TA?)
Strategy6 <- function(data=trainData) {
  
  
}








