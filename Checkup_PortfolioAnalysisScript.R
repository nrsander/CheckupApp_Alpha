
#################################
##  Portfolio Analysis Script  ##
#################################

source("Checkup_PortfolioCalculationFunctions.R")
library(tidyr)
require(zoo)


##############
#   INPUTS   #
##############

# Coins:
coins <- c("BTC","ETH","XMR","ZEC","NEO","GAS","ZRX","ETC")
time_period <- "minute"

# Date range:
trimDateRange <- FALSE
start_time <- "2018-07-07 12:00:00" # these only apply if TRUE
end_time <- "2018-07-08 00:00:00"

# Lags
k_Lags <- 1
k_Rolling <- 15



##===============================================================================================================================================================================================================
##===============================================================================================================================================================================================================



##############
#   SCRIPT   #
##############

# Download all coins' histo price data
myPrices_List <- HistoOHLCV(InputCoinSet=coins,timeframe=time_period,pd=2000)
# get Close price only
coinsPrices <- GetHistoClosesCombined(myPrices_List)

# Trim to custom date range?
if (trimDateRange == TRUE) {
  combodate <- paste0(start_time,"/",end_time)
  coinsPrices <- coinsPrices[combodate]
}

# Print Start, End dates
Print_StartEnd(coinsPrices)


#####


# Call the portfolio calc functions
myReturns <- Calculate_Returns(coinsPrices,k_Lags=k_Lags,TRUE) #  X
myLogReturns <- Calculate_LogReturns(coinsPrices,k_Lags=k_Lags,TRUE) #  X
myRollingReturns <- Calculate_RollingReturns(coinsPrices,k_Lags=k_Lags,k_Rolling=k_Rolling,TRUE) # Useful graph!
#myCumulativeReturns <- ...

# Count lost observations
cat("\nObservations:\n-------------\nStart:\t",nrow(myReturns),"\n  End:\t",nrow(myRollingReturns),"\n(Lost)\t",nrow(myReturns)-nrow(myRollingReturns),"\n-------------\n\n")

# Some other things that may be useful!
rollingbeta <- Calculate_BetaRolling(coinsPrices,selectCoins=c(2:ncol(coinsPrices),1),k_Lags=k_Lags,k_Rolling=k_Rolling,return=TRUE); print(plot(rollingbeta))
Corrs.beta <- Calculate_BetaCorrelations(coinsPrices,return=TRUE); print(Corrs.beta)
Corrs.rollingbeta <- Calculate_BetaRollingCorrelations(coinsPrices,selectCoins=c(1:min(ncol(coinsPrices),4)),k_Rolling=k_Rolling,return=TRUE); print(plot(Corrs.rollingbeta))


##===============================================================================================================================================================================================================


# Back to Rolling Returns ...
plot(myRollingReturns, main = "Rolling Returns")
addLegend("topleft", legend.names = names(myRollingReturns), ncol=3, lty=1, lwd=3, on=1, cex=0.6)

# Back to Log Returns ...
plot(myLogReturns, main = "Log Returns")
addLegend("topleft", legend.names = names(myLogReturns), ncol=3, lty=1, lwd=3, on=1, cex=0.6)




# Price (single coin)
#cIndex <- 4
#plot(coinsPrices[,cIndex], main = "Prices")
#addLegend("topleft", legend.names = names(coinsPrices)[cIndex], ncol=1, lty=1, lwd=3, on=1, cex=0.75)




#######################
##        RSI        ##
#######################

GetRSIs <- function(prices,ma_type="EMA",rsi_intervals=c(100,75,50,25,10)) {
  ## Logic:
  current_rsis_list <- list()
  for (u in 1:length(rsi_intervals)) {
    current_rsis <- data.frame(nrow=ncol(prices),ncol=2)
    for (i in 1:ncol(prices)) {
      rsi <- RSI(prices[,i],n=rsi_intervals[u],maType=ma_type)
      current_rsis[i,1] <- names(prices)[i]
      current_rsis[i,2] <- round(rsi[nrow(rsi)],0)
    }
    names(current_rsis) <- c("coin",paste0("RSI-",rsi_intervals[u]))
    current_rsis_list[[u]] <- current_rsis
  }
  names(current_rsis_list) <- paste0("RSI-",rsi_intervals)
  ## Convert list to df
  current_rsis_list <- current_rsis_list %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="coin"), .)
  ## Return:
  current_rsis_list
}

# Get RSIs
myRSIs <- GetRSIs(coinsPrices)
print(myRSIs)

# Slowly plot each coin's RSI at a few (5) different RSI-intervals
for (coin in 1:nrow(myRSIs)) {plot(x=(2:ncol(myRSIs))-1,y=myRSIs[coin,2:ncol(myRSIs)], type="l",ylab="RSI",xlab="RSI Interval (Long-term ---> short-term)",main=myRSIs$coin[a]); Sys.sleep(1)}






##################################################
##################################################
#                 v   v   v
#
#                   Copied
#                    From
#               _PortfolioStat
#        
#                 v   v   v
##################################################
##################################################

##                 ##
##  PortfolioStat  ##
##                 ##


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

