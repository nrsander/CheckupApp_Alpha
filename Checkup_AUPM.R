
##  
##  Always-Updating-Prices-Mode (AUPM)
##  (Live updates)
##  

source("Checkup_Dependencies.R")



Start_AUPM <- function(coin_tickers=c("BTC","ETH","LTC","XMR","NEO","ZRX")) {

  iLimit <- 1000 # <-- Editable
  
  # (infinite loop begins here)
  iCount <- 1
  running <- TRUE
  while(running) {
    cat("\n-------  Iteration ",iCount,"  -------\n",sep="")
    
    # First iteration: download initial data
    if (iCount==1) {
      data_New <- RemoveCloseFromColNames(GetHistoClosesCombined(HistoOHLCV(InputCoinSet=coin_tickers,PairCoin="USD",timeframe="minute",pd=60,showPlot=FALSE)))
    } else {
      data_Old <- data_New
      data_New <- RemoveCloseFromColNames(GetHistoClosesCombined(HistoOHLCV(InputCoinSet=coin_tickers,PairCoin="USD",timeframe="minute",pd=60,showPlot=FALSE)))
      
      # Calculate the difference btwn _Old and _New data
      minutesDiff <- (as.numeric(index(data_New[nrow(data_New)])) - as.numeric(index(data_Old[nrow(data_Old)])))/60 # minutes difference
      cat("Minutes difference:\t",minutesDiff,"\n")
      pricesDiff <- coredata(data_New)[nrow(coredata(data_New)),] - coredata(data_Old)[nrow(coredata(data_Old)),] # prices difference
      cat("Price differences:\n")
      print(pricesDiff); cat("\n\n"); Sys.sleep(1)
      
      # Print BTC price change
      if ((pricesDiff[1] == pricesDiff["BTC"])) {cat("BTC price change: \t",if(pricesDiff["BTC"]<0) {"$"} else {"$"},pricesDiff["BTC"],"\n",sep=""); Sys.sleep(0.5)} else {"N/a BTC\n"}
      
      
      ## ...
      ## if (!is.na(...)) {...}
      ## ...
      
    }
    
    
    # ...
    
    # Hit iteration limit yet?
    if (iCount<iLimit) {iCount<-iCount+1} else {running<-FALSE}
    
  } 
  1# (infinite loop ends here)
  
  # Print
  cat("\n[AUPM]\tSuccessfully completed",iCount-1,"iterations","\n[AUPM]\tTerminated\n")
}


Start_AUPM()



