############################################################
##  I don't think avg. rolling per k_Rolling timeframes   ##
## is telling me what I think it is during type="rolling" ##
############################################################
source("Checkup_PortfolioCalculationFunctions.R")



########################################################################
##                                                                    ##
##                         Compare Portfolios                         ##
##                                                                    ##
########################################################################

## NOTES:
## --------------------
##  *  Good way to show responsiveness to shocks:
##      *  type = rolling, timeframe = minute, and a low k_Rolling
##  *  rolling returns are currently hard-coded to use periodic returns, not log
##  *  rolling returns cannot go negative; need to diagnose why I did that
##      *  I think it was on purpose to show absolute relativity; might even be stationary
##



###################
##     Inputs    ##
###################

## Return calc. type
type = "rolling"

## Portfolios
#bagIndexes = c(1:4)
bags = LoadAllBags(hurry=TRUE)#[bagIndexes]
PairCoin = "USD"

## Time range
timeframe = "minute"
pd = 6 * 60
k_Rolling = 15 # (applies if type = rolling)
k_Lags = 1

## General settings
SavePNG = TRUE
showPlot = TRUE
hurry = TRUE



#######################
##       Script      ##
#######################

portfolios.returns <- ComparePortfolios(bags = bags, 
                                        type = type, 
                                        PairCoin = PairCoin, 
                                        timeframe = timeframe, 
                                        pd = pd, 
                                        k_Lags = k_Lags, 
                                        k_Rolling = k_Rolling,
                                        SavePNG = SavePNG, 
                                        showPlot = showPlot, 
                                        hurry = hurry)  





