#  _HistoPrices.R
#    _TSHistoPrices.R  or  _HistoOHLCV.R
#      _MasterListFunctions-v3_Veta.R       <---   ***
#        _New.R
source("Checkup_TSHistoPrices.R")




#===================\
# Create MList        \
#=======================\

MList <- function(InputCoinSet=c("BTC","ETH","LTC","BCH","ZRX"),PairCoin="USD",timeframe="day",pd=90,myTzone="EST",validate.xts=TRUE,rollingPd=30) {
  
  # Print
  cat("Provisioning MList\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  
  # Vectorize PairCoin
  PairCoin <- rep(PairCoin[1],length(InputCoinSet))
  
  # New list for collecting output
  Output.List <- list()
  
  # Begin building the components of MList ...
  currStep <- 0 # default = error
  numSteps <- 3 # total steps
  
  
  # Step 1 - Get List of OHLCV xts objects
  #          Element [[i]] represents coin i
  currStep <- Start_Step(1)
  beebo <- List_OHLCV.xts(InputCoinSet,PairCoin,timeframe,pd,myTzone,validate.xts)

  oMult <- do.call(merge, beebo)
  Output.List[[currStep]] <- oMult # (Multi-coin) [XTS] Nominal Prices ... over time (column for each O/H/L/C/V/V2 !! ****** !! not great but not bad. actually pretty bad)
  cat("\n100%")
  
  
  # Step 2
  cat("\nStep 2 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  Output.List[[2]] <- log(Output.List[[1]]) # (Multi-coin) [XTS] Log-scale Nominal Prices ... over time
  cat("\t100%")
  
  
  # Step 3
  cat("\nStep 3 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # calculate PERIODIC returns (every single pd)
  List_multiCoinPeriodicReturns <- list() # (each coin's returns-xts composes its own list element)
  for (i in 1:ncol(oMult)) {
    firstisna <- Delt(oMult[,i])
    List_multiCoinPeriodicReturns[[i]] <- firstisna[-1,]
    #names(List_multiCoinPeriodicReturns[[i]]) <- "Chg"
  }
  names(List_multiCoinPeriodicReturns) <- names(oMult)
  # merge multicoin returns List into multicoin returns xts
  oMultiReturns <- do.call(merge, List_multiCoinPeriodicReturns)
  names(oMultiReturns) <- names(List_multiCoinPeriodicReturns)
  Output.List[[3]] <- oMultiReturns # [Multi-coin XTS]  Returns over time
  cat("\t100%")
  
  
  
  # Set MList elements' names
  names(Output.List) <- c("Price","Log_Price","Periodic_Return")
  
  Output.List
  
  Sys.sleep(0.3)
  cat("\nMList has been provisioned.\n\n")
  # Return the MList
  Output.List
  
}

#=====================/
# End of MList       /
#===================/




# assist functions
#=============================================

Start_Step <- function(step=0) {
  if (step == 0) {
    cat("\n\n\t** Internal Error **\n\tBad Step Num.\n\tCoercing process result to Step 1\n\t(This will certainly cause errors elsewhere)\n\n")
  } else {
    cat("\nStep",step,"of",numSteps,"\t")
    for (i in 1:5) {Sys.sleep(0.1); cat(".")}
    cat("\n")
  }
  step
}







# Run Test
#=============================================

ddee <- MList()

# debug print
for (pp in 1:length(ddee)) {
  cat("\nElement ",pp," of ",length(ddee),":\n",sep="")
  Sys.sleep(0.5)
  cat(names(ddee)[pp],"\n")
  Sys.sleep(2)
  print(head(ddee[[pp]]))
  cat("\n")
  Sys.sleep(0.5)
}






#####
#####
###########
###########
###########
#####
#####


## not ready
#
##  1   -   Calculate cumulative (linear*) return.
#List_indivCoinCumReturns <- lapply(List_multiCoinPeriodicReturns, sum)
#names(List_indivCoinCumReturns) <- paste(c("BTC","ETH"),"Cum Return")
#
##  1.5 -   Running cumulative geometric returnS
#df_multiCoinPeriodicReturns <- do.call(merge,List_multiCoinPeriodicReturns)
#names(df_multiCoinPeriodicReturns) <- names(List_multiCoinPeriodicReturns)
#df_multiCoinPeriodicReturnsPlus1 <- df_multiCoinPeriodicReturns + 1
#plot(df_multiCoinPeriodicReturnsPlus1)
#
#rets <- df_multiCoinPeriodicReturnsPlus1
#for (x in 1:nrow(df_multiCoinPeriodicReturnsPlus1)) {
#  rets[x,1] <- prod()
#  # fuck it
#}
#
##  2   -   Calculate cumulative geometric return. (I THINK mathematically correct...)
#df_multiCoinPeriodicReturnsPlus1 <- apply(df_multiCoinPeriodicReturnsPlus1, 2, prod)
#names(df_multiCoinPeriodicReturnsPlus1) <- paste(c("BTC","ETH"),"Cum Return")
#
#
#head(df_multiCoinPeriodicReturns)
#