#  _HistoPrices.R
#    _TSHistoPrices.R
#      _MasterListFunctions.R    <---   ***
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

MasterList <- function(InputCoinSet=c("BTC","ETH","LTC"),PairCoin="USD",timeframe="hour",pd=1800,OHLC="C") {
 
  # Print
  cat("Creating MasterList\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  
  # List for collecting output
  Output.List <- list()
  
  # Vectorize PairCoin
  PairCoin <- rep(PairCoin,length(InputCoinSet))
  
  
  # Begin making MasterList components...
  
  cat("\nStep 1 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  
  singlecoin.prices.xts <- TSHistoPrices(InputCoinSet[1],PairCoin[1],timeframe,pd,OHLC)
  ##Output.List[[1]] <- singlecoin.prices.xts
  cat("Step 1\t100%\n")
  
  cat("\nStep 2 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  singlecoin.prices.df <- coredata(singlecoin.prices.xts)
  ##Output.List[[2]] <- singlecoin.prices.df
  cat("Step 2\t100%\n")
  
  cat("\nStep 3 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  #
  #@head(singlecoin.prices.xts)
  #@plot(singlecoin.prices.xts)
  
  
  # Multi Coin ----------
  #   1 -- Prices
  #         - xts
  #         - df
  #         > plot
  #   2 -- [Periodic] Returns
  #         - xts
  #         - df
  #         > plot
  #   3 -- Cumulative Returns ***
  #         - ???
  multicoin.prices.xts <- TSHistoPrices.Multi(InputCoinSet,PairCoin,timeframe,pd,OHLC)
  #@str(multicoin.prices.xts)
  #@head(multicoin.prices.xts[[1]])
  #@head(multicoin.prices.xts[[2]])
  #@str(multicoin.prices.xts[[1]])
  #@str(multicoin.prices.xts[[2]])
  oMult <- do.call(merge, multicoin.prices.xts)
  names(oMult) <- InputCoinSet
  ##Output.List[[3]] <- oMult
  cat("Step 3\t100%\n")
  
  cat("\nStep 4 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  oMult.df <- coredata(oMult)
  ##Output.List[[4]] <- oMult.df
  cat("Step 4\t100%\n")
  
  cat("\nStep 5 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  #
  #str(oMult)
  #head(oMult)
  #plot.xts(oMult) # BTC and ETH Price time series... on the same price scale. Absolute trash in graphical value!
  
  ### new list
  #dList <- list()
  #for (i in 1:length(multicoin.prices.xts)) {
  #  mlist[[i]] <-  999
  #}
  ### for i omult... add to list
  #btc.cd <- xts(oMult[,1],order.by = as.POSIXct(rownames(as.data.frame(oMult)))) # make a time series THAT APPROPRIATELY ACCOUNTS FOR DATETIMES!
  #eth.cd <- xts(oMult[,2],order.by = as.POSIXct(rownames(as.data.frame(oMult))))
  ##@plot.xts(btc.cd) # plot prices
  ##@plot.xts(eth.cd)
  
  
  # plot price chart of each coin (separately) for 1 sec each
  indivCoinPrices <- list()
  for (i in 1:ncol(oMult)) {
    indivCoinPrices[[i]] <- oMult[,i]
    #@print(plot.xts(indivCoinPrices[[i]],main=names(oMult)[i]))
    names(indivCoinPrices[[i]]) <- "Price"
    #@print(head(indivCoinPrices[[i]]))
    #@Sys.sleep(1)
  } # now disregard indivCoinPrices
  
  
  # calculate periodic returns 
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
  ##Output.List[[5]] <- oMultiReturns
  cat("Step 5\t100%\n")
  
  cat("\nStep 6 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  oMultiReturns.df <- coredata(oMultiReturns)
  ##Output.List[[6]] <- oMultiReturns.df
  cat("Step 6\t100%\n")
  
  cat("\nStep 7 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  
  ### The XTS de resistanz
  oMultiReturns
  #@head(oMultiReturns)
  #@str(oMultiReturns)
  #@plot(oMultiReturns) # ugly and crowded
  
  
  # Prices (single coin*)
  Output.List[[1]] <- singlecoin.prices.xts # (Single coin) [XTS] Nominal Price ... over time
  Output.List[[2]] <- singlecoin.prices.df # (Single coin) [DF] Nominal Price ... over time
  # Prices (multi-coin)
  Output.List[[3]] <- oMult # (Multi-coin) [XTS] Nominal Prices ... over time
  Output.List[[4]] <- oMult.df # (Multi-coin) [DF] Nominal Prices ... over time
  # Log Prices
  Output.List[[5]] <- log(Output.List[[3]]) # (Multi-coin) [XTS] Log-scale Prices ... over time
  Output.List[[6]] <- log(Output.List[[4]]) # (Multi-coin) [DF] Log-scale Prices ... over time
  # Returns
  Output.List[[7]] <- oMultiReturns # (Multi-coin) [XTS] Returns ... over time
  cat("Step 7\t100%\n")
  
  cat("\nStep 8 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  Output.List[[8]] <- oMultiReturns.df # (Multi-coin) [DF] Returns ... over time
  cat("Step 8\t100%\n")
  
  cat("\nStep 9 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  # Log Returns
  Output.List[[9]] <- diff(Output.List[[7]],lag=1)[-1,] # (Multi-coin) [XTS] Log-scale Returns ... over time
  cat("Step 9\t100%\n")
  
  cat("\nStep 10 of 10\t")
  for (i in 1:5) {Sys.sleep(0.2); cat(".")}
  cat("\n")
  Output.List[[10]] <- coredata(Output.List[[9]]) # (Multi-coin) [DF] Log-scale Returns ... over time
  
  # Set Output.List elements' names
  names(Output.List) <- c("Prices_Single.xts","Prices_Single.df","Prices_Multi.xts","Prices_Multi.df","Log_Prices.xts","Log_Prices.df","Returns.xts","Returns.df","Log_Returns.xts","Log_Returns.df")
  
  # Print to user that we're finished
  cat("Step 10\t100%\n\n")
  Sys.sleep(1)
  
  # Print a navigation guide to the MasterList for the user's benefit
  cat("How to navigate Master List:\n\t")
  for (i in 1:10) {Sys.sleep(0.2); cat("-")}
  cat(">\n\n")
  Sys.sleep(0.5)
  MasterListGuide()
  cat("MasterList successfully created.\n")
  # Return the MasterList
  Output.List
}

#=====================/
# End of MasterList  /
#===================/








MasterList.Guide <- function(oList=Output.List,returnNavDF=FALSE) {
  # Print a user-guide for navigation of Output.List
  message(".============================================.")
  message("||\t\tHow to Navigate Output.List ...\t\t||")
  message("'============================================'")
  user_guide.df <- data.frame("Index"=c(1:10),
                              "Element_Name"=c("Prices_Single.xts", "Prices_Single.df","Prices_Multi.xts","Prices_Multi.df","Log_Prices.xts","Log_Prices.df","Returns.xts","Returns.df","Log_Returns.xts","Log_Returns.df"))
  user_guide.df$Element_Name <- as.character(user_guide.df$Element_Name)
  for (i in 1:nrow(user_guide.df)) {
    message("<Output.List>[[",i,"]]\t\t\t<---  Elem. # ",i)
    message("      *\tName\t",user_guide.df$Element_Name[i])
    message("\t----\t-----------------")
    message("\tDesc\t",str_split(user_guide.df$Element_Name,pattern="\\.")[[i]][1])
    #message("\tType\t",str_split(user_guide.df$Element_Name,pattern="\\.")[[i]][2])
    message("\tType\t",if(str_split(user_guide.df$Element_Name,pattern="\\.")[[i]][2]=="df"){"df (Data frame)"} else {"xts (Time series)"})
    message("\tExample Row (Row #1)")
    print(Output.List[[i]][1,])
    
    #message("",user_guide.df$Element_Name)
    message("")
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