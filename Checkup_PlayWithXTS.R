###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###         PLAY WTIH         ###
###            XTS            ###
###                           ###
###   ---------------------   ###
###                           ###
###            for:           ###
###                           ###
###          TESTING          ###
###            XTS            ###
###   LOADING AND EXPORTING   ###
###                           ###
###         *FORMERLY*        ###
###        *ENHANCEXTS*       ###


# Delete this...

# OUR GLORIOUS TS!
head(userbags.xts) 

# Log
log.xts <- log(userbags.xts)
plot(log.xts)
log.xts




#  Script to test our XTS capabilities


#  Options:
#-------------------------------------------- #
#    Time Range...
myTf <- "day"
myPds <- 180  #  How many [myTf]s?
#    Plot...
useDefaultYAxis <- TRUE
customMinY <- 0.5
customMaxY <- 1.7
legendPosition <- "topleft"
#    Export?
EXPORT_BAG_TO_CSV <- FALSE
#




# Read in user input Bags
myBags <- Load_UserInputBags()
head(myBags)

# Generate the master Bag Returns xts
userBagReturns_Master.XTS <- Get_BagReturnsXTS(myListOfBags=myBags, denom="USD", timeframe_OHLCV=myTf, Periods=myPds, OHLC="C")
head(userBagReturns_Master.XTS)
plot(userBagReturns_Master.XTS)

# (preserve copies) #KEEP
userbags.xts <- userBagReturns_Master.XTS
userbags.df <- as.data.frame(coredata(userbags.xts))

# this is stupid
plot(Bag.Returns) # This only works for "day" because the time index is only a date. Somehow the time got cut off during xts creation. We need to fix that ASAP.

## Cycle thru each plot, display each for 1 second individually.
## -- --
for (i in 1:ncol(userbags.df)) {
  plot(userbags.df[,i],type="l",xlab="Time",ylab="Return",title=names(userbags.df)[i])
  Sys.sleep(1)
}
# (Helpful for "hour" and "minute" requests because this 
# strips the Date column (which would be wrongly plotting 
# a zombie stepwise graph across multiple days) and replaces 
# it with good ol' numerical indices)
## -- --


###----------------------###
###     - Plot 2.0 -     ###
###----------------------###


##########################
###    DIRTY DATING    ###
###   SO WE CAN PLOT   ###
##########################
### Add Date index the dirty way 
### (see bad code below) #
###                      #
######                   #
#####                    #
####                     #
###                      #
##                       #
# DOWNLOAD the data frame (numeric) of Bags' (undated but chronological) returns
#returnsDf <- Get_BagReturnsDF(myListOfBags=myListOfBags,denom=denom,timeframe_OHLCV=myTf,Periods=myPds,OHLC=OHLC)
returnsDf <- userbags.df #we already have it here
##                       #
# Find the dates associated with our returns data
backdoorGetDate <- MakeTimeSeries_Multiple(InputCoins=myListOfBags[[1]]$Coin[1],PairCoin=rep(denom,length(myListOfBags[[1]]$Coin)),timeframe_OHLCV=myTf,Periods=myPds,OHLC=OHLC)
bGD <- cbind(backdoorGetDate[1],returnsDf)[,-1] # Make row name into Date/Datetime rather than at time 1->t {using a backdoor method}
Date <- rownames(bGD)    #
##                       #
# Attach dates to the returns DF
dfWithDateCol <- data.frame(returnsDf,Date)
dfWithDateCol <- dfWithDateCol[,c(length(dfWithDateCol),2:length(dfWithDateCol)-1)] # reorder so date is first, not last
dfWithDateCol$Date <- as.Date(dfWithDateCol$Date) # make date object # SHOULD BE POSIX of some type *** but doesn't matter when using dates only (not specific times)
##                       #
# Create the xts (multiline) of Bags' % returns over selected period
Bag.Returns <- xts(dfWithDateCol[,-1],order.by = dfWithDateCol$Date)
plot(Bag.Returns)        #
##                       #
###                      #
####                     #
#####                    #
######                   #
#######                  #
##########################

# Stupid plot settings
AllOnSinglePanel <- TRUE # all lines on one chart, or separate?
par(mfrow = c(1, 1), mex = 0.8, cex = 0.5) # fiddle around stupidly with the margins

# Draw plot
if (useDefaultYAxis == TRUE) {
  plot(userbags.xts, xlab="Date", ylab="x Cumulative Gain/Loss",grid.ticks.on="months", major.ticks="months", multi.panel = !AllOnSinglePanel) # Labels don't work... **
} else {
  myYaxis <- c(customMinY,customMaxY)
  plot(userbags.xts, ylim=myYaxis, xlab="Date", ylab="x Cumulative Gain/Loss",grid.ticks.on="months", major.ticks="months", multi.panel = !AllOnSinglePanel) # Labels don't work... **
}
print(addLegend(legendPosition, legend.names=names(userbags.df), lty=5, lwd=5))

###----------------------###
###    - Export CSV -    ###
###----------------------###

# Export our combined Bag returns results as a .csv file to the Outputs folder
if (EXPORT_BAG_TO_CSV == TRUE) {
  Export_BagReturns_CSV(userbags.xts)
  ## ** - Exported data is Not Good, but Not Broken. 
  ## ** - But open the exported .csv. It clearly needs some TLC. 
  ## ** - Use coredata(bags.xts) and any other applicable methods to 
  ## ** - give the exported data an x axis and other improvements
}

