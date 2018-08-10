
#################################
##  Portfolio Analysis Script  ##
#################################

source("Checkup_PortfolioCalculationFunctions.R")
require(zoo)


##############
#   INPUTS   #
##############

# Coins:
coins <- c("BTC","ETH","XMR","ZEC","NEO","GAS","ZRX","ETC")
time_period <- "minute"

# Date range:  **
trimDateRange <- FALSE
start_time <- "2018-07-06 12:00:00" # these only apply if TRUE
end_time <- "2018-07-07 12:00:00"

# Lags
k_Lags <- 1
k_Rolling <- 30



##===============================================================================================================================================================================================================
##===============================================================================================================================================================================================================



##############
#   SCRIPT   #
##############

# Download all coins' histo price data
myPrices_List <- HistoOHLCV(InputCoinSet=coins,timeframe=time_period,pd=2000)

# Simplify all coins' histo price data to Close only
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
Corrs.rollingbeta <- Calculate_BetaRollingCorrelations(coinsPrices,selectCoins=c(1:4),k_Rolling=k_Rolling,return=TRUE); print(plot(Corrs.rollingbeta))



# ...



##===============================================================================================================================================================================================================









