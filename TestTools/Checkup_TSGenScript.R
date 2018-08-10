###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###           TSGen           ###
###           SCRIPT          ###



#=================================
#  SCRIPT INPUT
#=================================
# Coins to compare over time:
myCoins=c("BTC","TRX","XRP","XLM","BCH","ETC","ZEC")



#=================================
#  SINGLE TS                      (PRICE over time)
#=================================
# Generate ts
testTS <- MakeTimeSeries("BTC","USD","minute",360,"C")

# Plot ts
plot(testTS, col="orange", main="BTC-USD  -  Last 6 Hours")
# Plot SMA lines
lines(TTR::SMA(testTS, n = 20), col="blue") # SMA(20)  # stupid for minutes...
lines(TTR::SMA(testTS, n = 50), col="red", lty=2) # SMA(50)  # "
# Add Legend
addLegend("bottomleft", legend.names = c("Close", "SMA(20)", "SMA(50)"),
          lty=c(1, 1, 2), lwd=c(2, 1, 1), # (how to make smaller?)
          col=c("orange", "blue", "red"))



#=================================
#  RETURNS.COINS                  (Coins' % performance since initial date within the data)
#=================================
# Generate Returns.coins
Returns.coins <- Gen_tsBasket(myCoins=myCoins,denom="USD",timeframe_OHLCV="day",Periods=30,OHLC="C")

# Plot tsBasket
plot(Returns.coins)
# Add Legend
addLegend("topleft", legend.names = myCoins,
          lty=c(1, 1, 1), lwd=c(1, 1, 1)) # (how to make smaller?)








#__To-do:________________________________
##   Compare Relative BAG (/Portfolio) Performance
