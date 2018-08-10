###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###       CMC FUNCTIONS       ###


# Download CMC Current data
DownloadCMCData <- function(NumCoins=250) {
  ## Networking constants   (the CoinMarketCap URLs from which we shall scrape html tables)
  url_cmc <- "https://coinmarketcap.com/all/views/all/"
  url_cmc.hist <- "https://coinmarketcap.com/historical/" # "20171203/"    #^ Input validation should ensure historical date is +- n*7 days from the above example date, as CMC only takes historical snapshots weekly.
  
  message(".------------.--------------------------------------.")
  message("|  Download: |  Current CoinMarketCap market data   |")
  message("|            |  Top ",NumCoins," ranked cryptos              |")
  message("'------------'--------------------------------------'")
  message("")
  ## Connect & download
  message("Downloading ...")
  url_cmc %>% # load the data @ networking constant url
    read_html() %>%
    html_nodes(css = "table") %>%
    html_table() %>%
    as.data.frame() -> "tbl_cmc"
  ## Data cleaning
  tbl_cmc[] <- lapply(tbl_cmc, gsub, pattern = "\\\n|\\s|[%*$,?]", replacement = "")
  tbl_cmc$X. <- NULL # kill rank column
  tbl_cmc$Var.11 <- NULL # kill this weird new column
  names(tbl_cmc) <- c("name", "symb", "mcap", "price", "supply", "vol", "ch1h", "ch24h", "ch7d") # better col names
  tbl_cmc$name = substr(tbl_cmc$name,str_length(tbl_cmc$symb)+1,str_length(tbl_cmc$name)) # cut dumb extra text off, weird that this is needed
  # fix market cap column
  num_tbl_cmc <- lapply(tbl_cmc[-c(1:2)], as.numeric) %>% # turn mcap numeric
    as.data.frame()
  tbl_clean <- cbind(tbl_cmc$name, tbl_cmc$symb, num_tbl_cmc) #  <----  *****  All of these should be dropped from memory.
  names(tbl_clean) <- c("name", "symb", "mcap", "price", "supply", "vol", "ch1h", "ch24h", "ch7d")
  
  # Sort by mcap (desc.)
  top_X_mcap <- tbl_clean[order(tbl_clean$mcap, decreasing = TRUE),]
  
  # Print
  message("")
  message("Success:    CMC current crypto market data download complete.")
  message("            Top ",nrow(top_X_mcap)," Mkt. cap. cryptocurrencies are now available.")
  message("")
  
  # Limit to N
  top_X_mcap <- top_X_mcap[1:NumCoins,]
  
  # Return
  top_X_mcap
}


# Download CMC Historical data
DownloadCMCHistoricalData <- function(NumCoins=250,hist.date="20180701") {
  
  DisableWarnings()
  
  ## CMC-Historical URL
  url_cmc.hist <- paste0("https://coinmarketcap.com/historical/",hist.date)
  
  message(".------------.-----------------------------------------.")
  message("|  Download: |  Historical CoinMarketCap market data   |")
  message("|            |  Top ",NumCoins," ranked cryptos                 |")
  message("'------------'-----------------------------------------'")
  message("")
  ## Connect & download
  message("Downloading ...")
  url_cmc.hist %>% # load the data @ networking constant url
    read_html() %>%
    html_nodes(css = "table") %>%
    html_table() %>%
    as.data.frame() -> "tbl_cmc"
  ## Data cleaning
  tbl_cmc[] <- lapply(tbl_cmc, gsub, pattern = "\\\n|\\s|[%*$,?]", replacement = "")
  tbl_cmc$X. <- NULL # kill rank column
  tbl_cmc$Var.11 <- NULL # kill this weird new column
  names(tbl_cmc) <- c("name", "symb", "mcap", "price", "supply", "vol", "ch1h", "ch24h", "ch7d") # better col names
  tbl_cmc$name = substr(tbl_cmc$name,str_length(tbl_cmc$symb)+1,str_length(tbl_cmc$name)) # cut dumb extra text off, weird that this is needed
  # fix market cap column
  num_tbl_cmc <- lapply(tbl_cmc[-c(1:2)], as.numeric) %>% # turn mcap numeric
    as.data.frame()
  tbl_clean <- cbind(tbl_cmc$name, tbl_cmc$symb, num_tbl_cmc) #  <----  *****  All of these should be dropped from memory.
  names(tbl_clean) <- c("name", "symb", "mcap", "price", "supply", "vol", "ch1h", "ch24h", "ch7d")
  
  # Sort by mcap (desc.)
  top_X_mcap <- tbl_clean[order(tbl_clean$mcap, decreasing = TRUE),]
  
  # Print
  message("")
  message("Success:    CMC Historical market data download complete.")
  message("            Top ",nrow(top_X_mcap)," Mkt. cap. cryptocurrencies (as of histo. date) are now available.")
  message("")
  
  # Limit return set to N coins
  top_X_mcap <- top_X_mcap[1:NumCoins,]
  
  EnableWarnings()
  
  # Return
  top_X_mcap
}


# Get.Coin("ABC")
Get.Coin <- function(ticker) {
  df <- CMC[CMC$symb == ticker,]
  df
}


# Get.HotTopX
Get.HotTopX <- function(CoinsThruRank=100,TimePeriod="d",sortReverse=1) {
  if (TimePeriod =="h") {
    df <- arrange(.data = CMC[1:CoinsThruRank,], desc(ch1h))
    if (sortReverse == 1) {df <- arrange(.data = df, ch1h)}
  } else if (TimePeriod == "d") {
    df <- arrange(.data = CMC[1:CoinsThruRank,], desc(ch24h))
    if (sortReverse == 1) {df <- arrange(.data = df, ch24h)}
  } else if (TimePeriod == "w") {
    df <- arrange(.data = CMC[1:CoinsThruRank,], desc(ch7d))
    if (sortReverse == 1) {df <- arrange(.data = df, ch7d)}
  } else {
    df <- arrange(.data = CMC[1:100,], desc(ch24h))
  }
  df
}


# Snapshot CMC
SnapshotCMC <- function() {
  message("Attempting to take a CMC snapshot...")
  message("")
  tod <- today()
  fileName = paste(tod,"_Top",nrow(CMC),"CMC",".csv",sep="")
  message("Taking a snapshot of current CMC data...")
  file <- write.csv(CMC,fileName)
  message("Snapshot taken. Saving to the archive (/MarketSnapshots) ...")
  
  PrintSnapshotSuccessReport(tod,fileName,getwd())
}

