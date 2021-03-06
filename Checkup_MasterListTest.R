#  _HistoPrices.R
#    _TSHistoPrices.R
#      _MasterListFunctions.R
#        _MasterListTest.R    <---   ***
source("Checkup_MasterListFunctions.R")




#
##
###
####
#####
#@viz.xts
#@head(viz.xts)


#@Data <- cbind(diff(log(viz.xts[,1])),diff(log(viz.xts[,2])),diff(log(viz.xts[,3])),diff(log(viz.xts[,4])),diff(log(viz.xts[,5])),diff(log(viz.xts[,6])))

#@chart.Correlation(Data)
#viz.xts["201805200047/201805200050"]
#####
####
###
##
#




#  To-Do
#  '''''
#      +  Add a t/f option for log rather than normal returns
#      +  Add a Bags/Bag Returns-creation method inside the function call
#      +  And of course enable the application of weightings to these returns (*maybe)


# BTC   Black
# AION  Red
# NANO  Green
# KMD   Dark Blue
# AST   Light Blue
# MTL   Pink


##################
##  Parameters  ##
##################
#
# Data Settings:
Set_of_coins <- c("BTC","AION","RPX","NANO","XMR","ZEC") # ZRX
Denominator <- "USD"
Time_interval <- "minute"
Time_periods <- 24*60 # 1440 mins = 1 day  ==  1440 hours = 2 months (convenient!)
OHLC_choice <- "C"
#
# Simplification Settings:
RoundReturns_digits <- 3
#
# Visual Settings:
Pause_seconds<-0.1 # sleep N seconds between periods
RefreshPlot_every<-5 # refresh plot every M periods 
#       ^ Refresh rate *severely* affects processing time





##################
##   Function   ##
##################
VisualizeReturns <- function(rets,delaySecs=0.3,plotUpdateInterval=1,returnXTS=FALSE) {
  # estimate time required...
  nsecs <- nrow(rets)*delaySecs*(1/plotUpdateInterval)
  if (nsecs < 60) {
    nsecs <- nsecs
    nmin <- 0
  } else {
    nsecs <- mod(nsecs,60)
    nmin <- ((nrow(rets)*delaySecs*(1/plotUpdateInterval))-nsecs)/60
  }
  cat("\nEstimating time required...\n")
  for (i in 1:26) {cat("-");Sys.sleep(0.1)}
  cat("\n\tRoughly",nmin,"min,",nsecs,"sec\n")
  Sys.sleep(2)
  cat("\nInitializing...\n\n")
  Sys.sleep(2)
  # begin...
  for (xRow in 1:nrow(rets)) {
    for (xCol in 1:ncol(rets)) {
      if (xRow != 1) {rets[xRow,xCol] <- as.numeric(rets[xRow-1,xCol]) * as.numeric(rets[xRow,xCol])}
    }
    print(rets[xRow,])
    if (xRow != 1) {
      if (mod(xRow,plotUpdateInterval)==0) {
        #++ add a scrolling window option??
        print(plot.xts(rets[1:xRow,]))
        # Add Legend
        #@print(addLegend("bottomleft", legend.names = names(rets)))
        Sys.sleep(delaySecs) 
      }
    }
  }
  
  if (returnXTS == TRUE) {
    return(rets)
  }
}
# * - Vizualize(...) also *CREATES* the unique base-1 returns data frame that it produces. It isn't just a passthru convenience return


##################
##  Run Script  ##
##################
# Make a MasterList
myMasterList <- MasterList(InputCoinSet=Set_of_coins,PairCoin=Denominator,timeframe=Time_interval,pd=Time_periods,OHLC=OHLC_choice) #10 elements in master list
# Pull Returns (base 1.00)
myReturnsBase1 <- round(1 + myMasterList[[7]],RoundReturns_digits)
# Run Visualization
viz.xts <- VisualizeReturns(rets=myReturnsBase1,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE)
print(head(viz.xts))
message("MasterListTest complete.")


# for clarity...
# viz.xts == myMasterList[[7]] (and then converted to Base-1)



##################
##  AMAZING!!!  ##
##################


## testing...
#List_Returns_Iso <- list()
#for (i in 2:ncol(viz.xts)) {
#  List_Returns_Iso[[i-1]] <- viz.xts[,c(1,i)]
#  names(List_Returns_Iso)[[i-1]] <- names(viz.xts)[[i]]
#  print(head(List_Returns_Iso[[i-1]]))
#  print(plot(List_Returns_Iso[[i-1]])) # this should be rolling window.
#  Sys.sleep(2)
#}


###
###
###
quickTest <- function() {
  
  # [A] 3 Different calculation methods for display:
  periodic.returns_Base0 <- round(myMasterList[[7]],RoundReturns_digits)
  message("Three different methods of display calculation:")
  message("1.)  Base-0 Returns")
  print(periodic.returns_Base0[1,])
  periodic.returns_Base1 <- round(1 + myMasterList[[7]],RoundReturns_digits)
  message("2.)  Base-1 Returns")
  print(periodic.returns_Base1[1,])
  periodic.returns_Pct <- round(periodic.returns_Base0 * 100,3)
  message("3.)  'Percent' Returns")
  print(periodic.returns_Pct[1,])
  names(periodic.returns_Pct) <- paste0(names(periodic.returns_Base1)," %")
  message("")
  Sys.sleep(2)
  
  # [B] Isolations
  btc.eth <- List_Returns_Iso[[1]] ## taking ETH    # List of DFs, each DF is each coin's return isolated w/ BTC return
  print(head(btc.eth))
  print(plot(btc.eth))
  Sys.sleep(2)
  
  btc.eth$dif <- btc.eth[,2]-btc.eth[,1]
  print(plot(btc.eth$dif))
  btc.eth$logdif <- log(btc.eth[,2])-log(btc.eth[,1])
  btc.eth$zero <- 0
  print(plot(btc.eth))
  
}
#Sys.sleep(1)
#message("Running quickTest() ...")
#Sys.sleep(2)
#quickTest()
###
###
###







#########################################################
# When in doubt, refer to the MasterList user-guide:
#MasterListGuide(myMasterList,returnNavDF=FALSE)
#########################################################
