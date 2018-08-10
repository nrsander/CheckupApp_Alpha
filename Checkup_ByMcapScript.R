
#################
##   Inputs    ##
#################

# As of historical date
CMC_year <- "2018"
CMC_month <- "07"
CMC_day <- "01"

# Top N coins
N_coins <- 100

# Return calculation type
type <- "cum"

# Timeframe
pd <- 57 * 24
timeframe <- "hour"
k_Lags <- 1
k_Rolling <- 6

# Other settings
hurry <- TRUE
#useCustomList <- FALSE
#customList <- c("BTC","ETH","OOPS")


#################
##   Script    ##
#################

#if (!useCustomList) {
CMCHistoricalDate <- paste0(CMC_year,CMC_month,CMC_day)
cmc_hist <- DownloadCMCHistoricalData(NumCoins=N_coins, hist.date=CMCHistoricalDate)
## Get top 100 coins as of this date
histoCoins <- as.character(cmc_hist$symb)
#} else {
#  histoCoins <- customList
#}

## Bucketize
numBuckets <- 10
buckets <- list()
for (b in 1:numBuckets) {
  buckets[[b]] <- histoCoins[(((b-1)*numBuckets)+1):(((b-1)*numBuckets)+(length(histoCoins)/numBuckets))]
}

## Make each bucket into a formal Portfolio
portfolioNames <- c()
portfolios <- list()
for (p in 1:length(buckets)) {
  portfolioNames[p] <- paste0("Rank.",(((p-1)*numBuckets)+1),"-",(((p-1)*numBuckets)+(length(histoCoins)/numBuckets)))
  portfolios[[p]] <- MakePortfolio(InputCoinSet=buckets[[p]],weights=0,title=paste0("Coins: Mcap Rank ",(((p-1)*numBuckets)+1)," - ",(((p-1)*numBuckets)+(length(histoCoins)/numBuckets)),":"),type=type,k_Lags=k_Lags,k_Rolling=k_Rolling,tf=timeframe,pds=pd,showPlot=TRUE,hurry=hurry)
}

## Flatten into a single xts
df <- data.frame(matrix(unlist(portfolios), nrow=nrow(portfolios[[1]]), byrow=T))
xdf <- xts(df,order.by=index(portfolios[[1]]))
names(xdf) <- portfolioNames[1:ncol(xdf)]



#################
##   Results   ##
#################

tail(xdf)

ROI_range <- c(0.5,1.5)
plot(xdf,legend.loc = "topleft",main="Returns by Coin Rank\n\tTop 100 as of July 1, 2018",ylim=ROI_range)






