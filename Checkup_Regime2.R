
DisableWarnings()

#-
######################
##     INPUTS       ##
######################

# Coins
coins.tickers <- c("BTC")
pair.coin <- "USD"

# Time range
pd <- 30*24
timeframe <- "hour"
k_Lags <- 1

# Train-test dimensions
pctTrain <- 0.30
pctTest <- 0.70

# Delays (seconds)
minorSleep <- 2
majorSleep <- 2

# General settings
n_Regimes <- 2


#-
######################
##   DEPENDENCIES   ##
######################

startDateTimePOSIX <- Sys.time()-if (timeframe=="day"){pd*24*60*60}else if(timeframe=="minute"){pd*60}else{pd*60*60}


#-
##############################
##      GET PRICE DATA      ##
##############################

# Get close prices of all coins
coins.prices.OHLCV <- HistoOHLCV(InputCoinSet=coins.tickers, PairCoin=pair.coin, timeframe=timeframe, pd=pd)
coins.prices.close <- GetHistoClosesCombined(coins.prices.OHLCV)
coins.prices.close <- RemoveCloseFromColNames(coins.prices.close)

# Calculate periodic returns
coins.returns <- Calculate_Returns(coinsPrices = coins.prices.close, k_Lags = k_Lags)



# Train-test dimensions
nRows <- nrow(coins.returns)
nTrainRows <- round(pctTrain * nRows, 0)
nTestRows <- round(pctTest * nRows, 0)
if (nTrainRows + nTestRows != nRows) {nTrainRows <- nTrainRows + (nRows - (nTrainRows + nTestRows))} # if rounding error


# (Process each coin)
for (c in 1:length(coins.tickers)) {
  
  # "TRUE" Regimes       ##       ******************************************************************************** 
  true.states <- c(rep(1,nRows/4),rep(2,nRows/4),rep(1,nRows/4),rep(2,nRows/4))
  
  
  cat("\nProcessing\t",coins.tickers[c],"...\n")
  
  #-
  ##############################
  ##     SPLIT TRAIN-TEST     ##
  ##############################
  
  # Train set
  trainSet <- coins.returns[1:nTrainRows,c]
  
  # Test set
  testSet <- coins.returns[(nTrainRows+1):nRows,c]
  
  # Merge train & test
  fullSet <- merge.xts(trainSet[,1],testSet,join="outer")
  names(fullSet) <- c("Training","Testing")
  # Plot
  layout(1)
  plot(fullSet,main="Train-test Periodic Returns",legend.loc = "topleft")
  
  
  
  Sys.sleep(2)
  
  
  
  #-
  ######################
  ##   TRAINING SET   ##
  ######################
  
  # Find regimes of trainSet
  returns <- trainSet
  y=returns
  ResFit = HMMFit(y, nStates=n_Regimes)
  VitPath = viterbi(ResFit, y)
  
  # Forward-backward procedure; compute probabilities
  fb = forwardBackward(ResFit, y)
  
  # Plot probabilities and implied states
  layout(1:2)
  # 1
  plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
  legend(x='topright', c('ImpliedState1','ImpliedState2'),  fill=1:n_Regimes, bty='n')
  # 2
  matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
  legend(x='topright', c('ProbabilityState1','ProbabilityState2'),  fill=1:n_Regimes, bty='n')
  
  
  
  #-
  ######################
  ##   TESTING SET    ##
  ######################
  
  # Find regimes
  y <- testSet
  VitPath <- viterbi(ResFit, y)$states
  
  
  
  #-
  ######################
  ##   PREDICT ...    ##
  ######################
  
  ##
  ##
  ##  ----->    DOESN"T WORK    <-----  ##
  #<> inds <- merge(startDateTimePOSIX:index(coins.returns)[nRows],)
  ##
  ##
  
  
  # Data
  data = xts(y, as.POSIXct(inds))
  
  # Helper function
  RegimeColor <- function(cols) {
    for (i in 1:length(cols)) {
      if(cols[i]==3) {
        cols[i] <- 2
      } else if (cols[i]==2) {
        cols[i] <- 3
      } else {
        cols[i] <- cols[i]
      }
    }
    cols
  }
  
  # Plot same + Regime prediction results
  layout(1:3)
  # 1
  plota.control$col.x.highlight = col.add.alpha(RegimeColor(true.states+1), 150)
  plota(data, type='h', plotX=F, x.highlight=T)
  plota.legend('Returns + True Regimes')
  # 2
  plota.control$col.x.highlight = col.add.alpha(RegimeColor(true.states+1), 150)
  plota(cumprod(1+data/100), type='l', plotX=F, x.highlight=T)
  plota.legend('Equity + True Regimes')
  # 3
  plota.control$col.x.highlight = col.add.alpha(VitPath+1, 150)
  plota(data, type='h', x.highlight=T)
  plota.legend('Returns + Detected Regimes')
  
  # Re-enable warnings
  EnableWarnings()
  
  Sys.sleep(majorSleep)
  
}






