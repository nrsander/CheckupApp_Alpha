#  _HistoPrices.R
#    _HistoOHLCV.R
#      _MasterListFunctions-v3_Veta.R       <---   ***
#        _New.R
source("Checkup_HistoOHLCV.R")


#===================\
# Create MList        \
#=======================\

MList <- function(InputCoinSet=c("BTC","ETH","LTC","BCH","ZRX"),PairCoin="USD",timeframe="day",pd=90,myTzone="EST",validate.xts=TRUE,returns.lag=1,rollingPd=9) {
  
  #----------------------------------
  # Constants
  #----------------------------------
  # Semi-constants
  K_Lags_PeriodicReturns <- returns.lag
  # Formulaic constants
  PairCoin <- rep(PairCoin[1],length(InputCoinSet)) # Vectorize PairCoin

  #----------------------------------
  # Download & prepare data
  #----------------------------------
  # Step 0 - Initialize a List of OHLCV xts'es
  Print_Provisioning_MList()
  beebo <- List_OHLCV.xts(InputCoinSet,PairCoin,timeframe,pd,myTzone,validate.xts)
  oMult <- do.call(merge, beebo)
  Output.List <- list() # New list for collecting output
  # Step 0 (done)
  
  
  #----------------------------------
  # Start building MList
  #---------------------------------
  # Print
  cat("\nPreparing...\n\n")
  Sys.sleep(0.5)
  
  # Cycle thru steps
  numSteps <- 3 # total steps
  currStep <- 0 # default = error (consider -1)
  
  # Step 1 - Prices
  currStep <- Start_Step(1)
  Output.List[[currStep]] <- oMult # = Prices (nominal)
  
  # Step 2 - Log Prices
  currStep <- Start_Step(currStep+1)
  Output.List[[currStep]] <- log(Output.List[[1]]) # = Log Prices (nominal)
  
  # Step 3 - Periodic Returns  **
  currStep <- Start_Step(currStep+1)
  List_periodicReturns <- list()
  for (i in 1:ncol(oMult)) {List_periodicReturns[[i]] <- Delt(oMult[,i],k=K_Lags_PeriodicReturns)[-(1:K_Lags_PeriodicReturns),]}
  names(List_periodicReturns) <- names(oMult)
  oPeriodicReturns <- do.call(merge, List_periodicReturns) # merge periodic-returns-list components into an xts
  names(oPeriodicReturns) <- names(List_periodicReturns) # copy over names
  Output.List[[currStep]] <- oPeriodicReturns # = Periodic returns
  
  # Print
  cat("\t100%\n\nAll steps complete.\n")
  Sys.sleep(0.3)
  cat("MList has been provisioned.\n\n")
  Sys.sleep(0.3)
  
  # Name the MList elements
  
  names(Output.List) <- c("Price","Log_Price","Periodic_Returns")
  # Return the MList
  Output.List
  
}

#=====================/
# End of MList       /
#===================/




# Printing Functions
#=============================================
# Print some startup B.S.
Print_Provisioning_MList <- function() {
  cat("Provisioning MList\t")
  Sys.sleep(0.7)
  cat(" |")
  for (i in 1:5) {Sys.sleep(0.2); cat("-")}
  cat(">")
  Sys.sleep(0.2)
  cat("|")
  Sys.sleep(0.7)
  cat("\n")
}

# Print step starts & ends
Start_Step <- function(step=0) {
  
  # Prefix
  if (step == 0) {
    #Print_Error.BadStepNum()
    step <- step + 1 # Must be first run
    cat("")
  } else if (step == 1) {
    cat("")
  } else if (step > 1) {
    cat("\t100%")
  } else { # step is negative
    Print_Error.BadStepNum()
  }
  
  # Body
  cat("\nStep",step,"of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  
  # Postfix
  if (step == 0) {
    cat("")
  } else if (step == 1) {
    cat("")
  } else if (step > 1) {
    cat("")
  } else {
    Print_Error.BadStepNum()
  }
  
  # Return
  step
}

# Print error BadStepNum
Print_Error.BadStepNum <- function() {
  cat("\n\n\t**  Critical Error")
  cat("\n\t**  Bad Step Num")
  cat("\n\t**  This will certainly cause errors elsewhere.")
  Sys.sleep(1)
  cat("\nContinuing ")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  Sys.sleep(1)
  cat("\n\n")
}






# Run Test
#=============================================
# run
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