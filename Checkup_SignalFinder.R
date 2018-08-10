source("Checkup_PortfolioCalculationFunctions.R")
#####


##
##  This algo is meant to be used for 
##  cumulative (not log) returns, I think ...
##


##########
# INPUTS #
##########

# Coin:
inputCoin <- "BTC"

# Price time range:
pd <- 12*60
timeframe <- "minute"

# Algo settings:
lag <- 10           #   * * * * *
threshold <- 5      #    * * * * 
influence <- 0      #   * * * * *





##########
## Smoothed z-score algo (very robust thresholding algorithm)
##########
## This algo is based on the principle of dispersion: if a new datapoint is a given x number of standard deviations away from some moving mean, the algorithm signals (also called z-score). 
##    It is very robust because it constructs a separate moving mean and deviation, such that signals do not corrupt the threshold. 
##    Future signals are therefore identified with approximately the same accuracy, regardless of the amount of previous signals. 
##
## The algorithm takes 3 inputs: 
##    lag       = the lag of the moving window, 
##    threshold = the z-score at which the algorithm signals, and 
##    influence = the influence (between 0 and 1) of new signals on the mean and standard deviation. 
##
## For example, 
##    A lag of 5 will use the last 5 observations to smooth the data. 
##    A threshold of 3.5 will signal if a datapoint is 3.5 standard deviations away from the moving mean. 
##    And an influence of 0.5 gives signals half of the influence that normal datapoints have. 
##    Likewise, an influence of 0 ignores signals completely for recalculating the new threshold: 
##      an influence of 0 is therefore the most robust option; 1 is the least.
##########
## Other ways to modify the algorithm (for potential improvements) are:
##  -  Use median instead of mean
##  -  Use a robust measure of scale, such as the MAD, instead of the standard deviation
##  -  Use a signalling margin, so the signal doesn't switch too often
##  -  Change the way the influence parameter works
##  -  Treat up and down signals differently (asymmetric treatment)
##########



# Real code:
# ---------

# Algo function:
ThresholdingAlgo <- function(y,lag,threshold,influence) {
  y <- as.vector(y)
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i])
    stdFilter[i] <- sd(filteredY[(i-lag):i])
  }
  
  # Plot algo result (doesn't fuckin work)
  par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
  plot(1:length(y),y,type="l",ylab="",xlab="") 
  lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
  lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
  lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
  print(plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2))
  
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
  
}

###

# Get coins' close prices
inputCoin_closes <- GetHistoClosesCombined(HistoOHLCV(inputCoin,timeframe=timeframe,pd=pd))

# Calculate cumulative returns
inputCoin_cum_rets <- Calculate_CumReturns(coinsPrices=inputCoin_closes,return=TRUE) # Cumulative returns

# Run algo on a coin using inputs (y = Cum Returns)
result <- ThresholdingAlgo(y=inputCoin_cum_rets,lag,threshold,influence)

# Plot algo result
par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
plot(1:length(y),y,type="l",ylab="",xlab="") 
lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)






