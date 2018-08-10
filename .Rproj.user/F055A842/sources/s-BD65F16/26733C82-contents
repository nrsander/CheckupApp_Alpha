#  _HistoPrices.R
#    _TSHistoPrices.R
#      _MasterListFunctions2.R    <---   ***
#        _MasterListTest.R
source("Checkup_TSHistoPrices.R")


#~~testMasterList <- MasterList(InputCoinSet=c("BTC","ETH","XRP"),PairCoin="USD",timeframe="minute",pd=1800,OHLC="C")
#~~# print head of all 10 list elements
#~~for (i in 1:length(testMasterList)) {
#~~  print(head(testMasterList[[i]]))
#~~}


#===================\
# Create MasterList   \
#=======================\

MasterList <- function(InputCoinSet=c("BTC","ETH","LTC"),PairCoin="USD",timeframe="hour",pd=1800,OHLC="C",myTzone="UTC",rollingPd=30) {
 
  ### wait how many steps are we up to now??
  numSteps <- 11
  
  # Print
  cat("Provisioning MasterList\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  
  # Vectorize PairCoin
  PairCoin <- rep(PairCoin[1],length(InputCoinSet))
  
  # New list for collecting output
  Output.List <- list()

  
  # Begin building the components of MasterList ...
  
  cat("\nStep 1 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\n")
  singlecoin.prices.xts <- TSHistoPrices(InputCoinSet[1],PairCoin[1],timeframe,pd,OHLC,myTzone)
  # 100%
  
  cat("\nStep 2 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  cat("\n")
  multicoin.prices.xts <- TSHistoPrices.Multi(InputCoinSet,PairCoin,timeframe,pd,OHLC,myTzone)
  oMult <- do.call(merge, multicoin.prices.xts)
  names(oMult) <- InputCoinSet
  # 100%
  
  cat("\nStep 3 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  #--{indivCoinPrices <- list()
  #--{# while adding to list, also plot price chart of each coin (separately) for 1 sec each
  #--{for (i in 1:ncol(oMult)) {
  #--{  indivCoinPrices[[i]] <- oMult[,i]
  #--{  #@print(plot.xts(indivCoinPrices[[i]],main=names(oMult)[i]))
  #--{  names(indivCoinPrices[[i]]) <- "Price"
  #--{  #@print(head(indivCoinPrices[[i]]))
  #--{  #@Sys.sleep(1)
  #--{} # now disregard indivCoinPrices
  
  # calculate PERIODIC returns (every single pd)
  List_multiCoinPeriodicReturns <- list() # (each coin's returns-xts composes its own list element)
  for (i in 1:ncol(oMult)) {
    firstisna <- Delt(oMult[,i])
    List_multiCoinPeriodicReturns[[i]] <- firstisna[-1,]
    #@print(plot.xts(List_multiCoinPeriodicReturns[[i]],main=paste0(names(oMult)[i]," (Return)")))
    names(List_multiCoinPeriodicReturns[[i]]) <- "Chg"
    #@print(head(List_multiCoinPeriodicReturns[[i]]))
    #@Sys.sleep(1)
  }
  names(List_multiCoinPeriodicReturns) <- paste(InputCoinSet,"Returns")
  # merge multicoin returns List into multicoin returns xts
  oMultiReturns <- do.call(merge, List_multiCoinPeriodicReturns)
  names(oMultiReturns) <- paste(InputCoinSet,"Returns")
  cat("\t100%")
  
  
  cat("\nStep 4 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  
  ### The XTS de resistanz
  #@oMultiReturns
  #@head(oMultiReturns)
  #@str(oMultiReturns)
  #@plot(oMultiReturns) # ugly and crowded
  
  
  
  # start putting some into the list ...
  
  
  
  Output.List[[1]] <- singlecoin.prices.xts # (Single coin) [XTS] Nominal Price ... over time
  Output.List[[2]] <- oMult # (Multi-coin) [XTS] Nominal Prices ... over time
  Output.List[[3]] <- log(Output.List[[2]]) # (Multi-coin) [XTS] Log-scale Prices ... over time
  Output.List[[4]] <- oMultiReturns # [Multi-coin XTS]  Returns over time
  cat("\t100%")
  
  
  cat("\nStep 5 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Log Returns
  #Output.List[[5]] <- diff(Output.List[[4]],lag=1)[-1,] # [Multi-coin XTS]  Log-scale Returns over time   ## ************** ??????????? WHAT WAS I DOING???
  Output.List[[5]] <- log(Output.List[[4]]) # [Multi-coin XTS]  Log-scale Returns over time
  cat("\t100%")
  
  cat("\nStep 6 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Returns Base-1
  Output.List[[6]] <- round(1 + myMasterList[[4]],RoundReturns_digits) # [Multi-coin XTS]  Returns over time (Base-1)
  cat("\t100%")
  
  cat("\nStep 7 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Log Returns Base-1
  Output.List[[7]] <- round(1 + myMasterList[[5]],RoundReturns_digits) # [Multi-coin XTS]  Log-scale Returns over time (Base-1)
  cat("\t100%")
  
  
  
  cat("\nStep 8 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Rolling Returns (Base-1)
  cat("\tRolling window:\t",rollingPd," ",timeframe,"s",sep="")
  ## ...
  ## ...
  ## ...
  cat("\nStep TEST-STEP-A...\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  #--{indivCoinRollingRetsB0 <- list()
  #--{# while adding to list, also plot price chart of each coin (separately) for 1 sec each
  #--{for (i in 1:ncol(oMult)) {
  #--{  indivCoinRetsB0[[i]] <- oMult[,i]
  #--{  #@print(plot.xts(indivCoinPrices[[i]],main=names(oMult)[i]))
  #--{  names(indivCoinRetsB0[[i]]) <- paste0("Rolling ",rollingPd)
  #--{  #@print(head(indivCoinPrices[[i]]))
  #--{  #@Sys.sleep(1)
  #--{} # now disregard indivCoinPrices
  
  # calculate PERIODIC returns
  List_multiCoinRollingRets <- list() # (each coin's rolling-rets-xts composes its own list element)
  for (i in 1:ncol(oMult)) {
    firstisna <- Delt(oMult[,i],k=rollingPd)
    List_multiCoinRollingRets[[i]] <- firstisna[-c(1:rollingPd),]
    #@print(plot.xts(List_multiCoinRollingRets[[i]],main=paste0(names(oMult)[i]," (Roll Ret)")))
    names(List_multiCoinRollingRets[[i]]) <- "Roll. Rets."
    #@print(head(List_multiCoinRollingRets[[i]]))
    #@Sys.sleep(1)
  }
  names(List_multiCoinRollingRets) <- paste(InputCoinSet,"Roll. Ret.")
  # merge multicoin rolling rets List into multicoin rolling rets xts
  oMultiRollingRets <- do.call(merge, List_multiCoinRollingRets)
  names(oMultiRollingRets) <- paste(InputCoinSet,"Roll. Ret.")
  ## ...
  ## ...
  ## ...
  Output.List[[8]] <- oMultiRollingRets # [Multi-coin XTS]  Rolling Returns over time (Base-0)
  cat("\t100% TEST-STEP A (Roll Rets)")
  ##
  cat("\t100%")
  
  
  cat("\nStep 9 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Rolling Log Returns (Base-1)
  #@cat("\tRolling window:\t",rollingPd," ",timeframe,"s",sep="")
  ## ...
  ## ...
  ## ...
  Output.List[[9]] <- log(Output.List[[8]]) # [Multi-coin XTS]  Log-scale Returns over time (Base-0)
  # WAIT... IS THIS LEGAL WITH LOG SCALE?????? ******
  cat("\t100% TEST-STEP B (Log Roll Rets)")
  ## ...
  ## ...
  ## ...
  cat("\t100%")
  
  
  
  cat("\nStep 10 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Rolling Returns Base-1
  Output.List[[10]] <- round(1 + Output.List[[8]],RoundReturns_digits) # [Multi-coin XTS]  Returns over time (Base-1)
  cat("\t100%")
  
  cat("\nStep 11 of",numSteps,"\t")
  for (i in 1:5) {Sys.sleep(0.1); cat(".")}
  # Log Rolling Returns Base-1
  Output.List[[11]] <- round(1 + Output.List[[9]],RoundReturns_digits) # [Multi-coin XTS]  Log-scale Returns over time (Base-1)
  # WAIT... IS THIS LEGAL WITH LOG SCALE?????? ******
  cat("\t100%")
  
  
  
  # Set Output.List elements' names
  names(Output.List) <- c("Prices_Single.xts","Prices_Multi.xts","Log_Prices.xts","Returns.xts","Log_Returns.xts","Returns_Base1.xts","Log_Returns_Base1.xts","Rolling_Returns.xts","Rolling_Log_Returns.xts","Rolling_Returns_Base1.xts","Rolling_Log_Returns_Base1.xts")
  
  # Print to user that we're finished
  cat("\n\n")
  Sys.sleep(1)
  
  # Print a navigation guide to the MasterList for the user's benefit
  cat("MasterList Contents\t")
  for (i in 1:10) {Sys.sleep(0.1); cat("-")}
  cat(">\n\n")
  Sys.sleep(0.3)
  MasterList.Guide(Output.List,returnNavDF = FALSE)
  Sys.sleep(0.3)
  cat("MasterList has been provisioned.\n\n")
  # Return the MasterList
  Output.List
}

#=====================/
# End of MasterList  /
#===================/








MasterList.Guide <- function(oList=Output.List,returnNavDF=FALSE) {
  # Print a user-guide for navigation of Output.List
  message(".===============================================,")
  message("||\t\tHow to Navigate Output.List ...\t||")
  message("'==============================================='")
  user_guide.df <- data.frame("Index"=c(1:length(oList)),
                              "Element_Name"=names(oList))
  user_guide.df$Element_Name <- as.character(user_guide.df$Element_Name)
  for (i in 1:nrow(user_guide.df)) {
    message("<MasterList>[[",i,"]]\t\t\t<-- Elem. # ",i)
    message("      *\tName\t",user_guide.df$Element_Name[i])
    message("\t----- \t-----------------")
    message("\tDesc :\t",str_split(user_guide.df$Element_Name,pattern="\\.")[[i]][1])
    #message("\tType\t",str_split(user_guide.df$Element_Name,pattern="\\.")[[i]][2])
    message("\tType :\t",if(str_split(user_guide.df$Element_Name,pattern="\\.")[[i]][2]=="df"){"df (Data frame)"} else {"xts (Time series)"})
    message("\tRow 1:")
    print(oList[[i]][1,])
    
    #message("",user_guide.df$Element_Name)
    message("")
    Sys.sleep(0.1)
  }
  message("")
  print(user_guide.df)
  message("")
  message("Good luck!")
  message("")
  if (returnNavDF == TRUE) {return(user_guide.df)}
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