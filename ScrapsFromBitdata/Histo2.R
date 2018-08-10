############################################################################
###                     LETS BUILD A DATA SET !!!!!!                     ###
############################################################################


########################
###      Input       ###
########################

#   What coins do you want to include in the data set?

myCoins = c("ETH","LTC","XLM", "ZRX")



#######################
###   Dependencies  ###
#######################
source("HistoFunctions.R")

#      library(TTR)


########################
###     CoinsData    ###
########################
CoinsData <- BuildCoinData(Bag=myCoins,PairCoin="BTC",pd=1000)

# read out a little bit
for (i in 1:length(CoinsData)) {
  message("Coin ",i,": ",names(CoinsData)[i])
  close <- (get("close", CoinsData[[i]]))
  message("Mean USD price: $",round(mean(close) * GetBTCAtTimestamp(),2), "             ","Mean BTC price: ",round(mean(close),8), " BTC")
}















#t <- get("time", CoinsData[[1]])
#t
#c <- get("close", CoinsData[[1]])
#c

#head(CoinsData[[1]])
#View(CoinsData[[1]])
#View(CoinsData[[2]])
#View(CoinsData[[...]])
#View(CoinsData[[N]])












#pp.secs <- df$time
#pp.mins <- df$time/60
#pp.hrs <- df$min/60
#pp.days <- df$hrs/24
#pp.weeks <- df$days/7
#pp.years <- df$days*365
#pp.months <- df$years/12
