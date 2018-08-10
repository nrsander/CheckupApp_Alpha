#cmc <- DownloadCMCData(500)  #  download only if unavailable
#####


################
##   Inputs   ##
################

# Input coins
input_coins <- c("BTC","ETH","BCH","LTC")
input_weights <- c(0.6403,0.2613,0.0722,0.0262)

# Time range (historical prices)
tf <- "hour"
pds <- 30 * 24

# Run script
RUN_CPR <- TRUE



###################################################################



#################
##  Functions  ##
#################

# Calculate a portfolio's (weighted) return
Calculate_PortfolioReturn <- function(InputCoinSet, weights = 0, PairCoin = "USD", timeframe = "hour", pd = 30*24) {
  
  ##   Validate Inputs  
  #########################
  
  ## Exclude these coins:
  excludeCoins <- c("USDT","TUSD")
  
  ## ... other validation ...
  
  
  ##  Calculate           
  ##  Cumulative Returns  
  ##########################
  
  # Download historical close prices
  my.coins.prices <- GetHistoClosesCombined(HistoOHLCV(InputCoinSet=InputCoinSet,PairCoin=PairCoin,timeframe=timeframe,pd=pd))
  
  # Calculate cumulative returns
  my.cum.rets <- Calculate_CumReturns(my.coins.prices)
  # Plot cum rets
  plot(my.cum.rets, legend.loc = c("topleft"), y.intersp = 0, lty = 1, bty = "n", cex = .4, main="Cumulative Returns", auto.legend = T)
  cat("\n[Plotting] \tCumulative returns by coin\n");Sys.sleep(0.5); for (i in 1:3) {cat("\t...");Sys.sleep(0.5)}
  
  # Get returns to-date (full duration of timeframe)
  my.rets.toDate <- round(my.cum.rets[nrow(my.cum.rets),],4)
  # Print total returns to date
  cat(paste("\nTotal returns to date:\n"));Sys.sleep(0.5)
  print(my.rets.toDate); for (i in 1:3) {cat("\t...");Sys.sleep(0.5)}
  
  
  ##  Calculate Weighted  
  ##  Portfolio Return    
  ##########################
  
  ## Use default or custom weights?
  if (weights==0) {
    cat("\nWarning:\tPortfolio is unweighted.\n\tContinuing with default weighting instead (equal) ...\n")
    defaultWeighted <- TRUE
    weights <- rep(1/ncol(my.cum.rets),ncol(my.cum.rets))
  } else {
    if (length(weights) != length(InputCoinSet)) {
      cat("\nError:\tIncorrect number of weights\n\t# coins =",length(InputCoinSet),",","# weights =",length(weights),"\n\tContinuing using default weighting instead (all equal) ...")
      defaultWeighted <- TRUE
      weights <- rep(1/ncol(my.cum.rets),ncol(my.cum.rets))
    } else {
      cat("\nCustom weighting scheme detected\n");Sys.sleep(1)
      defaultWeighted <- FALSE
    }
  }
  ## Apply weights then sum
  portfolio.cum.rets <-  rowSums((my.cum.rets * weights))
  portfolio.cum.rets <- xts(portfolio.cum.rets,order.by=index(my.cum.rets))
  ## Plot weighted portfolio return
  plot(portfolio.cum.rets, y.intersp = 0, lty = 1, bty = "n", cex = .5, main="Portfolio Return", auto.legend = T)
  cat("\n[Plotting] \t",if(!defaultWeighted){"Weighted "},"Portfolio Return\n",sep="");Sys.sleep(0.5); for (i in 1:3) {cat("\t...");Sys.sleep(0.5)}
  ## Return
  portfolio.cum.rets
}



##################
##    Script    ##
##################

# test run
if (RUN_CPR == TRUE) {
  rez <- Calculate_PortfolioReturn(InputCoinSet = input_coins, weights = input_weights, PairCoin = "USD", timeframe = tf, pd = pds)
  plot(rez)
}





