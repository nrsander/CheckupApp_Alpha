###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###          START            ###


# ----------------------- #
# GLOBAL WARNINGS SETTING #
# ----------------------- #
options(warn=1) # On      #
options(warn=0) # Off     #
# ----------------------- #
# * Dangerous!



source("Checkup_Dependencies.R")

## RUN which modules?
RUN_EnhanceXTS <- 1

RUN_CMCScript <- 1
RUN_CCScript <- 1
RUN_CCHistScript <- 1
RUN_PortfolioScript <- 1
RUN_TSGenScript <- 1

#\\\\\\\\\\\\\\\\\\\\\\\\\\
if (RUN_EnhanceXTS == 1) {
  RUN_CMCScript <- 0
  RUN_CCScript <- 0
  RUN_CCHistScript <- 0
  RUN_PortfolioScript <- 0
  RUN_TSGenScript <- 0
}
#//////////////////////////




message("### - * - * - * - * - * - * - * - ###")
message("### *   C H E C K U P           * ###")
message("### *   S T A R T I N G . . .   * ###")
message("### * - * - * - * - * - * - * * - ###")
message("")

##############################
##       Run CMCScript      ##
##############################
if (RUN_CMCScript == 1) {
  ## Run CMCScript
  source("Checkup_CMCScript.R")
}

##############################
##       Run CCScript       ##
##############################
if (RUN_CCScript == 1) {
  #----------------------------------------------------
  ## Binance Bag (current):
  myBag <- c("GNT","ZRX","MTL","ICX","STRAT","KMD","STEEM","LSK","BNB","BTC")
  myQtys <- c(5000,1000,300,280,250,200,200,150,10.1,0.0089)
  ##myWeights <- ##must be calculated somehow after determining total USD/BTC value
  #----------------------------------------------------
  ## CCScript Parameters:
  daysOfData <- 3           #   Horizon of time:   {daysOfData} days ago    ---->      Current time
  myDenomination <- "USD"   #   Options:           "USD"   |   "BTC" ***               *** (BTC is not working yet... causes problems to PortfolioFunctions)
  myTimeframe <- "hour"     #   Options:           "day"   |   "hour"   |   "minute"   Get OHLCV data at every {myTimeframe}
  #----------------------------------------------------
  ## Run CCScript
  source("Checkup_CCScript.R")   #  'TestBag'  <----  A List of DF's   ... one for each coin in myBag ... each DF contains OHLCV data at row's time/date
}

##############################
##     Run CCHistScript     ##
##############################
if (RUN_CCHistScript == 1) {
  ## Run CCHistScript
  source("Checkup_CCHistScript.R")
}

##############################
##   Run PortfolioScript    ##
##############################
if (RUN_PortfolioScript == 1) {
  ## Construct Portfolio
  portfolio <- ConstructPortfolio(myBag,myQtys)
  ## Run PortfolioScript
  source("Checkup_PortfolioScript.R")
}

##############################
##     Run TSGenScript      ##
##############################
if (RUN_TSGenScript == 1) {
  ## Run TSGenScript
  source("Checkup_TSGenScript.R")
}


##############################
##      Run EnhanceXTS      ##
##############################
if (RUN_EnhanceXTS == 1) {
  ## Run EnhanceXTS
  source("Checkup_EnhanceXTS.R")
}




####################################
##       Suppress Warnings        ##
####################################
#Not working:
#lapply(tbl_cmc[-c(1:2)], as.numeric) : NAs introduced by coercion


message("")
message("")
message("### - * - * - * - * - * - * - * - ###")
message("### *   C H E C K U P           * ###")
message("### *   C O M P L E T E .       * ###")
message("### * - * - * - * - * - * - * - * ###")
message("")

