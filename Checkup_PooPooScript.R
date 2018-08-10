

###testMasterList <- MasterList(iCoins=c("BTC","ETH","XRP"),iPairCoin="USD",timeF="minute",periodsN=1800,ohlcX="C")
# print head of all 10 list elements
#for (i in 1:length(testMasterList)) {
#  print(head(testMasterList[[i]]))
#}

#==================================\
# Start PooPooScript                \
#====================================\

MasterList <- function(iCoins=c("BTC","ETH","LTC"),iPairCoin="USD",timeF="hour",periodsN=1800,ohlcX="C") {
  # List for collecting output
  Output.List <- list()
  
  # Vectorize iPairCoin
  iPairCoin <- rep(iPairCoin,length(iCoins))
  
  ##timeF <- "minute" # day minute hour
  ##periodsN <- 30*60 # < 2000    1800 mins  =  30 hrs  =  1.25 days
  ##ohlcX <- "C" #OHLC
  
  
  # Single Coin ----------
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
  singlecoin.prices.xts <- MakeTimeSeries(iCoins[1],iPairCoin[1],timeF,periodsN,ohlcX)
  ##Output.List[[1]] <- singlecoin.prices.xts
  singlecoin.prices.df <- coredata(singlecoin.prices.xts)
  ##Output.List[[2]] <- singlecoin.prices.df
  #
  head(singlecoin.prices.xts)
  plot(singlecoin.prices.xts)
  
  
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
  multicoin.prices.xts <- MakeTimeSeries_Multiple(iCoins,iPairCoin,timeF,periodsN,ohlcX)
  #str(multicoin.prices.xts)
  #head(multicoin.prices.xts[[1]])
  #head(multicoin.prices.xts[[2]])
  #str(multicoin.prices.xts[[1]])
  #str(multicoin.prices.xts[[2]])
  oMult <- do.call(merge, multicoin.prices.xts)
  names(oMult) <- iCoins
  ##Output.List[[3]] <- oMult
  oMult.df <- coredata(oMult)
  ##Output.List[[4]] <- oMult.df
  
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
  #plot.xts(btc.cd) # plot prices
  #plot.xts(eth.cd)
  
  
  # plot price chart of each coin (separately) for 1 sec each
  indivCoinPrices <- list()
  for (i in 1:ncol(oMult)) {
    indivCoinPrices[[i]] <- oMult[,i]
    print(plot.xts(indivCoinPrices[[i]],main=names(oMult)[i]))
    names(indivCoinPrices[[i]]) <- "Price"
    print(head(indivCoinPrices[[i]]))
    Sys.sleep(1)
  } # now disregard indivCoinPrices
  
  
  # calculate periodic returns 
  List_multiCoinPeriodicReturns <- list() # (each coin's returns-xts composes its own list element)
  for (i in 1:ncol(oMult)) {
    firstisna <- Delt(oMult[,i])
    List_multiCoinPeriodicReturns[[i]] <- firstisna[-1,]
    print(plot.xts(List_multiCoinPeriodicReturns[[i]],main=paste0(names(oMult)[i]," (Return)")))
    names(List_multiCoinPeriodicReturns[[i]]) <- "Chg"
    print(head(List_multiCoinPeriodicReturns[[i]]))
    Sys.sleep(1)
  }
  names(List_multiCoinPeriodicReturns) <- paste(iCoins,"Returns")
  
  # merge multicoin returns List into multicoin returns xts
  oMultiReturns <- do.call(merge, List_multiCoinPeriodicReturns)
  names(oMultiReturns) <- paste(iCoins,"Returns")
  ##Output.List[[5]] <- oMultiReturns
  oMultiReturns.df <- coredata(oMultiReturns)
  ##Output.List[[6]] <- oMultiReturns.df
  
  
  ### The XTS de resistanz
  oMultiReturns
  head(oMultiReturns)
  str(oMultiReturns)
  plot(oMultiReturns) # ugly and crowded
  
  
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
  Output.List[[8]] <- oMultiReturns.df # (Multi-coin) [DF] Returns ... over time
  # Log Returns
  Output.List[[9]] <- diff(Output.List[[7]],lag=1)[-1,] # (Multi-coin) [XTS] Log-scale Returns ... over time
  Output.List[[10]] <- coredata(Output.List[[9]]) # (Multi-coin) [DF] Log-scale Returns ... over time
  
  # Set Output.List elements' names
  names(Output.List) <- c("Prices_Single.xts","Prices_Single.df","Prices_Multi.xts","Prices_Multi.df","Log_Prices.xts","Log_Prices.df","Returns.xts","Returns.df","Log_Returns.xts","Log_Returns.df")
  
  # Print a navigation guide to this list for the user
  print_OutputListNavigationGuide()
  
  Output.List
}

#testMasterList <- MasterList()
#str(testMasterList)
## print head of all 10 list elements
#for (i in 1:length(testMasterList)) {
#  print(head(testMasterList[[i]]))
#}







print_OutputListNavigationGuide <- function(oList=Output.List,returnNavDF=FALSE) {
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
#====================================/
# End PooPooScript                  /
#==================================/




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
#
##  2   -   Calculate cumulative geometric return. (I THINK mathematically correct...)
#df_multiCoinPeriodicReturnsPlus1 <- apply(df_multiCoinPeriodicReturnsPlus1, 2, prod)
#names(df_multiCoinPeriodicReturnsPlus1) <- paste(c("BTC","ETH"),"Cum Return")
#
#
#head(df_multiCoinPeriodicReturns)
#