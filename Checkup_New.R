##################
##  Parameters  ##
##################

# Data Settings:
Set_of_coins <- c("BTC","AION","RPX","NANO","XMR","ZEC") # ZRX
Denominator <- "USD"
Time_interval <- "minute"
Time_periods <- 24*60 # 1440 mins = 1 day  ==  1440 hours = 2 months (convenient!)
OHLC_choice <- "C"
myTimezone <- "EST"

# Beta:
Rolling_returns_window <- 15

# Simplification Settings:
RoundReturns_digits <- 3

# Visual Settings:
Pause_seconds<-0.1 # sleep N seconds between periods
RefreshPlot_every<-5 # refresh plot every M periods 
#       ^ Refresh rate *severely* affects processing time


# START TESTING 1
scrollingPct <- 0.05 # Rolling window (% of periods)
# END TESTING 1


##################
##   Function   ##
##################
VisualizeReturns <- function(rets,delaySecs=0.3,plotUpdateInterval=1,returnXTS=FALSE,scrollingPct=FALSE,rollingPct=FALSE) {
  
  cat("\nInitializing...\n")
  for (i in 1:15) {cat("-");Sys.sleep(0.1)}
  Sys.sleep(0.6)
  
  # estimate time required...
  #nsecs <- nrow(rets)*delaySecs*(1/plotUpdateInterval)
  #if (nsecs < 60) {
  #  nsecs <- nsecs
  #  nmin <- 0
  #} else {
  #  nsecs <- mod(nsecs,60)
  #  nmin <- ((nrow(rets)*delaySecs*(1/plotUpdateInterval))-nsecs)/60
  #}
  #cat("\nEstimating time required...\n")
  #cat("\n\tRoughly",nmin,"min,",nsecs,"sec\n")
  #Sys.sleep(2)
  
  
  # Begin Logic-----
    
  
  # (but did user enter one?)
  if (scrollingPct != FALSE) {
    scrollingPds <- round(nrow(rets) * scrollingPct,0) # actual Scrolling window amt.
  }
  # (but did user enter one?)
  if (rollingPct != FALSE) {
    rollingPds <- round(nrow(rets) * rollingPct,0) # actual Returns = Rolling returns window amt.
  }
  
  
  
  
  
  # For each period of data available.........
  for (xRow in 1:nrow(rets)) {
    cat("\n")
    
    # For each coin's returns value in this period.........
    for (xCol in 1:ncol(rets)) {
      
      #-- What you're *defining/assigning* (modifying/transforming the returns in some manner) --#
      if (rollingPct != FALSE) {
        if (xRow > rollingPds) { # (start collecting returns as soon as rolling window begins)
          # >>>>>>>> bbbVal <- rets[(xRow-rollingPds),xCol] * rets[xRow,xCol]
          # >>>>>>>> rets[xRow,xCol] <- as.numeric(bbbVal) # Returns = returns over rolling window
          rets[xRow,xCol] <- as.numeric(rets[(xRow-rollingPds),xCol] * rets[xRow,xCol])      # I THINK THIS IS WRONG ... ******
        }
      } else {
        if (xRow > 1) { # (start collecting returns as soon as a prior period exists... aka period 2)
          rets[xRow,xCol] <- as.numeric(rets[xRow-1,xCol] * rets[xRow,xCol]) # Returns = simple old return since last period (lag=1)      # I THINK THIS IS WRONG ... ******
          #rets[xRow,xCol] <- as.numeric((rets[xRow-1,xCol] * rets[xRow,xCol])/rets[xRow-1,xCol]) # Returns = simple old return since last period (lag=1)
          # >>>>>>>> rets[xRow,xCol] <- as.numeric(rets[xRow-1,xCol] * rets[xRow,xCol])
        }
      }
      
    }
    
    #-- Print add this row's defined data
    print(rets[xRow,])
    
    #-- Are we updating plot at this interval?
    if (mod(xRow,plotUpdateInterval)==0) {
      
      #-- What you're *plotting/saving*: --#
      if (scrollingPct != FALSE) { # (make sure we can scroll back far enough to plot)
        if (xRow > scrollingPds) {
          if (rollingPct != FALSE) { # (make sure there's data available)
            if (xRow > rollingPds) {
              print(plot.xts(rets[(xRow-scrollingPds):xRow,])) # Plot the modified returns with a scrolling window
            }
          }
          print(plot.xts(rets[(xRow-scrollingPds):xRow,])) # ("^) Plot the modified returns with a scrolling window
        }
      } else {
        if (xRow > 1) {
          print(plot.xts(rets[1:xRow,])) # Plot the simple old running cumulative all-time returns 
        }
      }
      
      # Legend?
      #@print(addLegend("bottomleft", legend.names = names(rets)))
      
      # Delay for user
      Sys.sleep(delaySecs) 
      
    }
  }
    
    
  if (returnXTS == TRUE) {
    return(rets)
    # Vizualize(...) 
    #> *CREATES and RETURNS the MODIFIED / TRANSFORMED 
    #> Returns data set (df)*. 
    #> It isn't just a pretty plot function 
    #> or a passthru convenience return of original returns
  }
}




#
##
###
##################
##  Run Script  ##
##################


#
# THERE IS A PROBLEM WITH
# THE DIFFERENCING...
#
# OTHER FUNCTION ALREADY DOES IT >:(
#



# Make a MasterList
                # ( *** if necessary *** )
myMasterList <- MasterList(InputCoinSet=Set_of_coins,PairCoin=Denominator,timeframe=Time_interval,pd=Time_periods,OHLC=OHLC_choice,myTzone=myTimezone,rollingPd=Rolling_returns_window) #10 elements in master list

# Pull elements from MasterList
#@myPriceHist <- myMasterList[[2]] # Pull --> Prices
#@myLogPriceHist <- myMasterList[[3]] # Pull --> Log Prices
#@myReturnsBase1 <- myMasterList[[6]] # Pull --> Returns (Base-1)
#@myLogReturnsBase1 <- myMasterList[[7]] # Pull --> Log Returns (Base-1)


myTest <- myMasterList[[2]] # Prices


vizTest1 <- VisualizeReturns(rets=myTest,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE,rollingPct=FALSE,scrollingPct=(0.01042*10))





myTest2 <- myMasterList[[10]] # Rolling Returns Base 1



# Run visualizations

plot(myTest2)
vizTest2 <- VisualizeReturns(rets=myTest,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE,rollingPct=FALSE,scrollingPct=(0.01042*10))

vizVol <- VisualizeReturns(rets=myPriceHist,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE,rollingPct=0.018,scrollingPct=FALSE)
vizLogVol <- VisualizeReturns(rets=myLogPriceHist,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE,rollingPct=0.018,scrollingPct=FALSE)

vizVolatility <- VisualizeReturns(rets=myReturnsBase1,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE,rollingPct=0.018,scrollingPct=FALSE)
vizLogVolatility <- VisualizeReturns(rets=myLogReturnsBase1,delaySecs=Pause_seconds,plotUpdateInterval=RefreshPlot_every,returnXTS=TRUE,rollingPct=0.018,scrollingPct=FALSE)




#          #
##        ##
###      ###
####    ####
#####  #####
####    ####
###      ###
##        ##
#          #





message("MasterListTest complete.")