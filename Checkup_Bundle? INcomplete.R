# Test data set
testBundle <- myShitcoinPriceBundle
View(testBundle)

#####################################



## Calc RETURNS
#CalcBundleReturns <- function(myBundle) {
#  rets <- c()
#  for (i in 1:length(myBundle)) {
#    rets[i] <- names(myBundle)[i]
#  }
#  rets
#}
#testTickers <- CoinsInBundle(testBundle)

QM_INDEX <- 4

for (iCoin in 1:length(myBundle)) {
  output <- c()
  
  # Plot coin
  bund <- myBundle[[iCoin]][[QM_INDEX]]
  names(bund)
  has.Vo(bund)
  chartSeries(bund, name = names(myBundle)[iCoin], type = "auto", bar.type = "ohlc", TA = 'addVo()', theme = chartTheme("black"))
  Sys.sleep(1)
  
  # Hi/Lo of whole period
  hi <- seriesHi(bund)
  lo <- seriesLo(bund)
  hilo <- c(hi, lo)
 
  
  Sys.sleep(1)
}

addBBands(n = 50, sd = 2, ma = "SMA", draw = 'bands', on = -1)












#####################################

# Returns a string vector of coin tickers in bundle
CoinsInBundle <- function(myBundle) {
  vec <- c()
  for (i in 1:length(myBundle)) {vec[i] <- names(myBundle)[i]}
  vec
}
testTickers <- CoinsInBundle(testBundle)




