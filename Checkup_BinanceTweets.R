
############
## Inputs ##
############
watchlist_accounts <- c("Binance")
N_tweets = 3000
REFRESH_CMC_DATA <- FALSE



#####################
## Get Ticker Data ##
#####################
## Download tickers from CMC
if (REFRESH_CMC_DATA) {tkrs <- DownloadCMCData(500)}
tkrs500 <- as.character(tkrs$symb)



################
## Get Tweets ##
################
# Download past tweets from @binance
binance <- get_timelines(watchlist_accounts, n = N_tweets)
binance %>%
  #dplyr::filter(created_at > "2018-02-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Binance tweets",
    subtitle = "Tweet counts aggregated by day (since Feb 1, 2018)",
    caption = "\nSource: DunceBot"
  )



########################
##  Cashtag Analysis  ##
########################

## subset tweets to those that contain non-hashtag symbols
b_tweets <- binance[!is.na(binance$symbols),]
## Print how many
for (wa in 1:length(watchlist_accounts)) {cat("\n",watchlist_accounts[wa],"\n\tTotal tweets: \t",nrow(binance),"\n","\tMay hv ctags: \t",nrow(b_tweets),"\t(",round(nrow(b_tweets)/nrow(binance),3)*100,"%)\n",sep="");Sys.sleep(1.5)}


## extract $symbols only
possible_cashtags <- unlist(b_tweets$symbols)
cashtags_used <- cashtags_used_unique <- c()

## check if each symbol really is a cashtag (=is a valid coin symbol and at least in the top 500 mcap)
for (poss_ct in 1:length(possible_cashtags)) {
  if (possible_cashtags[poss_ct] %in% tkrs500) {
    cashtags_used[length(cashtags_used)+1] <- possible_cashtags[poss_ct]
    if (possible_cashtags[poss_ct] %in% cashtags_used_unique) {
      #already had this coin
    } else {
      cashtags_used_unique[length(cashtags_used_unique)+1] <- possible_cashtags[poss_ct]
    }
  }
}


#
###
#####  -----  b_coins  -----,
#######                     |
#########  Download historical price data for every coin @binance has ever tweeted about
#b_coins <- HistoOHLCV(InputCoinSet=cashtags_used_unique,PairCoin="BTC",timeframe="hour",pd=2000)
#######                     | # perma_b_coins <- b_coins
#####  -----  b_coins  -----'
###
#





#################################################################################################

## Test vs a subset of tweets
subset_b_tweets <- b_tweets[1:44,] # (tweets since April 27th)

###########

## Collect output
plottweet_returns_list <- list()

## For each binance tweet with 1 or more cashtags ...
for (bTweet in 1:nrow(subset_b_tweets)) {
  
  ## Tweet index (i of N)
  cat("\n","Tweet ",bTweet,":\tTime = ",sep="")
  
  ## Time of tweet (timestamp):
  cat(subset_b_tweets$created_at[bTweet],"\n",sep="")
  ts <- subset_b_tweets$created_at[bTweet]
  
  ## Header
  cat("\n\t\tCashtag\n",sep="")
  
  ## For each cashtag mentioned in this specific tweet ...
  for (bCashtag in 1:length(subset_b_tweets$symbols[bTweet][[1]])) {
    
    ## Cashtag index
    cat("\t(",length(subset_b_tweets$symbols[bTweet]),"/",bCashtag,")",sep="")
    
    ## Cashtag itself
    cat("\t",subset_b_tweets$symbols[bTweet][[1]][[bCashtag]],sep="")
    ctg <- subset_b_tweets$symbols[bTweet][[1]][[bCashtag]]
    
    ## Price-response analysis
    cat("\n\n\tPrice-response analysis:\n\t\t...\n\n")
    
    ##  ---------------------  (Testing)  ---------------------  ##
    
    ## setup
    test_coin <- toupper(ctg)
    test <- b_coins[test_coin]
    test_qm <- test[[1]]$qm
    test_qm.xts <- xts(test_qm)
    secsDelayPlot <- 7
    
    ## determine plot range
    ts.nearestMin <- as.POSIXct(round.POSIXt(ts,"mins"))
    ts.nearestHour <- as.POSIXct(round.POSIXt(ts,"hours"))
    #plotrange <- paste0(date(ts.nearestHour)) # full day of tweet
    plotrange <- paste0(ts.nearestHour-(12*60*60),"/",ts.nearestHour+(12*60*60)) # full day of tweet
    tweet.timeonly <- str_sub(str_split(as.character(ts.nearestMin)," ")[[1]][2],1,5)
    
    ## xts version
    plottweet <- test_qm.xts[plotrange]
    
    ## qm version
    plottweet_qm <- as.quantmod.OHLC(plottweet)
    names(plottweet_qm) <- names(plottweet)
    #plot(plottweet_qm[,c(4:5)],main=paste0(ctg," Price & Volatility\n   @binance tweeted at ",tweet.timeonly)) # Close, Volatility (in BTC)
    #Sys.sleep(secsDelayPlot)
    
    
    
    ## Line vertically on graph showing tweet time
    ## (...)
    
    ## Line on the same chart comparing returns to BTC returns
    ## (...)
    
    
    
    ## close price only
    plottweet_close <- plottweet_qm[,4]
    
    ## Calculate returns around tweet-time
    cat("\n\t",ctg,"\t",tweet.timeonly,"\n")
    plottweet_returns <- Calculate_CumReturns(plottweet_close)
    
    ## Plot
    plot(plottweet_returns)
    title(paste0(ctg," % return"), line = -1)
    title(sub = paste0("@binance tweeted at ", tweet.timeonly), line = -2)
    
    Sys.sleep(secsDelayPlot)

    ## Set output
    plottweet_returns_list[[length(plottweet_returns_list)+1]] <- plottweet_returns
    
    
    
    
    
    
    ## see price action before and after tweet (plot xts version)
    #print(plot(plottweet[,2:3],main=paste0(ctg," High & Low\n    @binance tweeted at ",tweet.timeonly),cex=0.5)) # High, Low
    #Sys.sleep(secsDelayPlot)
    
    
    ##############################
    
    # ...
    
    
    ##############################
    ## (time difference btwn. actual-tweet time and price-captured time)
    #timeFromTweetToPrice <- (ts.nearestHour-ts.nearestMin)
    ## (get price at [closest point to] tweet time)
    #priceAtTweetTime <- test_qm[ts.nearestHour,]
    
  }
}













## Print each cashtag used
#cat("\n\nFrequency of cashtag use:\n");Sys.sleep(1);print(cashtags_used);Sys.sleep(1)
## Print uniques only
#cat("\n\nUnique cashtags used:\n");Sys.sleep(1);print(cashtags_used_unique);Sys.sleep(1)




#View(b_tweets)










# view
#View(binance)












## GET TIMELINES:

## get user IDs of accounts followed by CNN
#binance <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 3200)

## plot the frequency of tweets for each user over time
#binance %>%
#  dplyr::filter(created_at > "2017-10-29") %>%
#  dplyr::group_by(screen_name) %>%
#  ts_plot("days", trim = 1L) +
#  ggplot2::geom_point() +
#  ggplot2::theme_minimal() +
#  ggplot2::theme(
#    legend.title = ggplot2::element_blank(),
#    legend.position = "bottom",
#    plot.title = ggplot2::element_text(face = "bold")) +
#  ggplot2::labs(
#    x = NULL, y = NULL,
#    title = "Frequency of Twitter statuses posted by news organization",
#    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
#    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#  )
