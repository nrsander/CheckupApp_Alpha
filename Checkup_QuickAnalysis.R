
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

# Time range:  **
start_time <- "2018-07-06 12:00:00"
end_time <- "2018-07-07 12:00:00"

# Lags
k_Lags <- 1
k_Rolling <- 60



##===============================================================================================================================================================================================================
##===============================================================================================================================================================================================================



##############
#   SCRIPT   #
##############

# Get price data
myPrices_List <- HistoOHLCV(InputCoinSet=coins,timeframe=time_period,pd=2000) # Download histo prices
combined_Closes <- GetHistoClosesCombined(myPrices_List) # Combine the coins' Close vectors into one xts
combodate <- paste0(start_time,"/",end_time); combined_Closes.CustomTimeRange <- combined_Closes[combodate] # Trim to custom time range
coinsPrices <- combined_Closes.CustomTimeRange # Set default data

# Print Start and End times
Print_StartEnd(combined_Closes)
cat("\n")
Print_StartEnd(combined_Closes.CustomTimeRange)
cat("\n\n")
cat("Rows (pre-) : ",nrow(combined_Closes),"\n",sep="") # num rows (pre-)
cat("Rows (post-): ",nrow(combined_Closes.CustomTimeRange),"\n\n",sep="") # num rows (post-)


#####


# Call the portfolio calc functions
myReturns <- Calculate_Returns(coinsPrices,k_Lags=k_Lags,TRUE) #  X
myLogReturns <- Calculate_LogReturns(coinsPrices,k_Lags=k_Lags,TRUE) #  X
myRollingReturns <- Calculate_RollingReturns(coinsPrices,k_Lags=k_Lags,k_Rolling=k_Rolling,TRUE) # Useful graph!
#myCumulativeReturns <- ...

# Plot some things that may be useful
Calculate_BetaCorrelations(coinsPrices)
Calculate_BetaRolling(coinsPrices,selectCoins=c(2:ncol(coinsPrices),1),k_Lags=k_Lags,k_Rolling=k_Rolling,return=FALSE) # Useful!   (Order of coins matters)
Calculate_BetaRollingCorrelations(coinsPrices,selectCoins=c(1:4),k_Rolling=k_Rolling,return=FALSE) # Useful!   (Order doesn't matter)


# ...


##===============================================================================================================================================================================================================




