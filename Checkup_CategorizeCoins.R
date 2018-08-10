
# 
# ----------
# Categorize
# Coins
# ----------
# 

###############
#  Functions  #
###############

GetCategorization <- function(n_coins=400,print=FALSE) {
  #
  # NOTE: This should NOT have to be downloaded every time.
  #       Very poor design.
  #
  
  # Download CMC data for the Top n_coins
  myLargeCoinData <- DownloadCMCData(n_coins)
  
  # Vectors
  symb <- myLargeCoinData$symb
  mcap <- myLargeCoinData$mcap
  price <- myLargeCoinData$price
  supply <- myLargeCoinData$supply
  
  # Constants
  million <- 1000000
  billion <- 1000*million
  satoshi <- 0.00000001
  
  # (Cat 1)  Sort by mcap ($)
  mcap_cut <- cut(mcap, breaks = c(0,10*million,25*million,50*million,100*million,500*million,1000*million,round(myLargeCoinData$mcap[1]+5001*million,-10)))
  mcap_table <- table(mcap_cut)
  # (Cat 2)  Sort by Mkt Cap Rank
  rank <- rev(rank(mcap))
  rank_cut <- cut(rank, breaks = c(0,10,25,50,100,200,max(rank)))
  rank_table <- table(rank_cut)
  # (Cat 3)  Sort by nominal price ($)
  price_cut.USD <- cut(price, breaks = c(0,0.03,0.10,0.25,0.50,1,10,25,100,500,1000,10000))
  price_table.USD <- table(price_cut.USD)
  # (Cat 4)  Sort by nominal price (BTC)
  price_cut.BTC <- cut(price/myLargeCoinData$price[1], breaks = c(0,100*satoshi,500*satoshi,1000*satoshi,5000*satoshi,10000*satoshi,100000*satoshi,1000000*satoshi,5000000*satoshi,1.01*100000000*satoshi))
  price_table.BTC <- table(price_cut.BTC)
  # (Cat 5)  Sort by total coin supply
  maxSupply <- 1000*billion
  supply_cut <- cut(supply, breaks = c(0,10*million,50*million,100*million,500*million,1*billion,10*billion,maxSupply))
  supply_table <- table(supply_cut)

  if (print==TRUE) {
    # Print tables (quantity w/in each category)
    print(mcap_table)
    print(rank_table)
    print(price_table.USD)
    print(price_table.BTC)
    print(supply_table)
    # Print statistical summaries
    print(summary(mcap))
    print(summary(rank))
    print(summary(price))
    print(summary(supply))
  }
  
  # Create the Categorization df
  categorization <- data.frame(myLargeCoinData$name, myLargeCoinData$symb, mcap_cut, rank_cut, price_cut.USD, price_cut.BTC, supply_cut)
  names(categorization) <- c("name","symb","Cat_mcap","Cat_rank","Cat_price.USD","Cat_price.BTC","Cat_supply")
  head(categorization)
  
  # Return
  categorization
}
# ---
SaveCategorization <- function(cats) {
  filename="Categorization.csv"
  write.table(x=cats, file=filename, na="", quote=FALSE, row.names=FALSE, sep="|") # write .csv
  cat("\n",filename,"scheme saved.")
}
ReadCategorization <- function(filename="Categorization.csv") {
  cats <- read.csv(file=filename,sep="|")
  cat("\n",filename," opened.")
  cats
}
ReadCoinTickers <- function(filename="Coin_Tickers.csv") {
  ctkrs <- read.csv(file=filename)
  cat("\n",filename," opened.")
  ctkrs
}
ReadCoinCatListA <- function(filename="CoinCategoryList_A.csv") {
  ccList_A <- read.csv(file=filename)
  cat("\n",filename," opened.")
  ccList_A
}

############
#  Script  #
############


RunMergeScript <- function(n_Coins=400) {
  
  # Create a new bucketized categorization scheme
  myCategories <- GetCategorization(n_Coins)
  
  # Save .csv categorization scheme
  SaveCategorization(myCategories)
  
  # ---
  
  ### Bucketized categories
  myCategories <- ReadCategorization()  # filename = "Categorization.csv"
  ### ...then modified in Excel
  myMiscCats <- ReadCoinTickers()  # filename = "Coin_Tickers.csv"
  names(myMiscCats) <- c("symb","name","Purpose1","Purpose2","Regional","ConsensusAlgo","xchg-Binance","xchg-Poloniex","xchg-Huobi")
  # Partial merge
  semiMatchedCats <- merge(myMiscCats,myCategories,by=c("symb","name"))

  # ---
  
  ### External-source
  myCoinCatListA <- ReadCoinCatListA() # filename = "CoinCategoryList_A.csv"
  names(myCoinCatListA) <- c("name","symb","Category")
  # Full merge
  fullyMatchedCats <- merge(semiMatchedCats,myCoinCatListA,by=c("symb","name"))

  # ---
  
  # Print
  cat("\n\n\t(A)\n\t",nrow(myCategories),"(B)\n\t",nrow(semiMatchedCats),"(C)\n\t",nrow(fullyMatchedCats))
  
  # Return
  fullyMatchedCats
  
}


#fullyMatchedCats <- RunMergeScript()
#fullyMatchedCats


#
# ...
#




