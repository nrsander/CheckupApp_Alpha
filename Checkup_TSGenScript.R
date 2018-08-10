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

##
####
######   Still needs to be printed out on paper
####
##

#=================================
#  SCRIPT INPUT                   
#  ---> Which coins to observe over time?
#=================================
# Coins to compare over time:
myCoins=c("BTC","ETH","LTC","BCH")



#=================================
#  COIN.RETURNS                   
#  ---> % Performance of multiple coins
#       (since initial observation)
#=================================
# Generate Coin.Returns
Coin.Returns <- Get_CoinReturns(myCoins=myCoins,denom="USD",timeframe_OHLCV="day",Periods=30,OHLC="C")

# Plot Coin.Returns
plot(Coin.Returns)
# Add Legend
addLegend("topleft", legend.names = myCoins,
          lty=c(1, 1, 1), lwd=c(1, 1, 1)) # (how to make smaller?)





#=================================
#  SINGLE TS                      (One coin's PRICE over time)
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


