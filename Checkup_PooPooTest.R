#source("Checkup_MasterListFunctions.R")
#
# List user-manual   -->
print_OutputListNavigationGuide(myMasterList,returnNavDF=FALSE)



##################
##  Parameters  ##
##################
Set_of_coins <- c("BTC","ETH","LTC","BCH","XRP","XLM","ZRX")
Denominator <- "USD"
Time_interval <- "minute"
Time_periods <- 24*60 # 1440 mins = 1 day
OHLC_choice <- "C"



##################
##  Run Script  ##
##################
# Initialize a MasterList
myMasterList <- MasterList(iCoins=Set_of_coins,iPairCoin="USD",timeF=Time_interval,periodsN=Time_periods,ohlcX=OHLC_choice) #10 elements in master list
# Pull Returns (Base 1.00)
testoB1 <- round(1 + myMasterList[[7]],8)
# Run Viz
visualizeReturns(rets=testoB1,delaySecs=0.1,plotUpdateInterval=5,returnXTS=FALSE)



##################
##   Function   ##
##################
visualizeReturns <- function(rets,delaySecs=0.3,plotUpdateInterval=1,returnXTS=FALSE) {
  # estimate time required...
  nsecs <- nrow(rets)*delaySecs*(1/plotUpdateInterval)
  if (nsecs < 60) {
    nsecs <- nsecs
    nmin <- 0
  } else {
    nsecs <- mod(nsecs,60)
    nmin <- ((nrow(rets)*delaySecs*(1/plotUpdateInterval))-nsecs)/60
  }
  message("Estimated time required:  ",nmin," min, ",nsecs," sec")
  message("-----------------------")
  message("")
  Sys.sleep(2)
  message("Initializing...")
  Sys.sleep(1)
  message("")
  # begin...
  for (xRow in 1:nrow(testoB1)) {
    for (xCol in 1:ncol(testoB1)) {
      if (xRow != 1) {testoB1[xRow,xCol] <- as.numeric(testoB1[xRow-1,xCol]) * as.numeric(testoB1[xRow,xCol])}
    }
    print(testoB1[xRow,])
    if (xRow != 1) {
      if (mod(xRow,plotUpdateInterval)==0) {
        
        #++ add a scrolling window option??
        print(plot.xts(testoB1[1:xRow,]))
        Sys.sleep(delaySecs) 
      }
    }
  }
  
  if (returnXTS == TRUE) {
    return(testoB1)
  }
}

##################
##  AMAZING!!!  ##
##################









#######



# func def
#++ add a Bags/Bag Returns creation method inside function call
#++ and of course add weightings to these bag returns













# 3 Different methods of calculation/display
periodic.returns_Base0 <- round(myMasterList[[7]],4) # *******
periodic.returns_Base1 <- round(1 + myMasterList[[7]],4)
periodic.returns_Pct <- round(periodic.returns_Base0 * 100,2)
names(periodic.returns_Pct) <- paste0(names(periodic.returns_Base1)," %")

# Compare the 3 plots (all the same...?)
##plot.xts(periodic.returns_Base1)
##head(periodic.returns_Base0)
##Sys.sleep(1)
##plot.xts(periodic.returns_Base1)
##head(periodic.returns_Base1)
##Sys.sleep(1)
#plot.xts(periodic.returns_Pct)
#head(periodic.returns_Pct)
#Sys.sleep(1)
