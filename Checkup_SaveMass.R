
# SaveMass

setwd("~/Desktop/CRYPTO LYFE/Checkup/CheckupApp")
source("Checkup_Dependencies.R")


########################
#        Script        #
########################

# Download CMC data for the Top x coins
myLargeCoinData <- DownloadCMCData(300)

# Top50 Subset (1-50)
myTop50 <- myLargeCoinData[1:50,]
# Shitcoin Subset (101-200)
#myShitcoins <- myLargeCoinData[101:nrow(myLargeCoinData),]

print(head(myTop50)) # preview
cat("\n")
#(but we dont even use the data...)

# Get just the symbs
mySymbVec <- as.character(myTop50$symb)

# Adjustments (testing purposes)
for (s in 1:length(mySymbVec)) {
  if (mySymbVec[s] == "MIOTA") {mySymbVec[s] <- "IOT"}
}
for (s in 1:length(mySymbVec)) {
  if (mySymbVec[s] == "HOT") {mySymbVec[s] <- mySymbVec[s-1]}
}

# Run PRICE-BUNDLER script on the Top x coins
myShitcoinPriceBundle <- HistoOHLCV(InputCoinSet=mySymbVec,PairCoin="USD",timeframe="minute",pd=2000,myTzone="UTC",showPlot=TRUE)
  
# Run SAVE SCRIPT on the bundle
mySaveScriptResult <- SavePrices(myShitcoinPriceBundle)
# (^ that's null idiot...)









