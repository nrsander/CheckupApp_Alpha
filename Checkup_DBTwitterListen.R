##############
##  Source  ##
##############
source("Checkup_DBTwitterFunctions.R")



##################
## ,----------, ##
## |  Inputs  | ##
## '----------' ##
##################

##----------------------------##
## Stream settings            ##
##----------------------------##
NEW_STREAM <- TRUE
recorded_tweets_file <- "Test_TweetStream.json"
stream_time <- 180
VIEW_NOW <- TRUE

##----------------------------##
## Terms to listen for        ##
##----------------------------##
terms <- "Bitcoin, Ethereum, Cryptocurrencies, $BTC, $ETH, Binance, Poloniex, Coinbase"  #  (one string, comma-sep)

##----------------------------##
## Watchlist                  ##
##----------------------------##
watchlist.exchanges <- c("coinbase",
                         "bittrex",
                         "binance",
                         "cz_binance",
                         "poloniex",
                         "bitfinex",
                         "huobi")
watchlist.news <- c("coindesk",
                    "cointelegraph",
                    "crypto")
watchlist.test <- c("almetaware9",
                    "YassinMobarak",
                    "BestBuy")

watchlist.me <- c("o_hoops","bryptopia")
watchlist <- c(watchlist.exchanges,watchlist.news,watchlist.test,watchlist.me)

##----------------------------##
## Limit tweetset data cols   ##
##----------------------------##
keep_columns <- c("created_at",
                  "screen_name",
                  "verified",
                  "is_retweet",
                  "text",
                  "hashtags",
                  "symbols",
                  "followers_count")



#####################
##  Stream Tweets  ##
#####################

## Listen for new tweets
if (NEW_STREAM == TRUE) {
  cat("\n\nStarting new stream ...\n\tWriting to: ",recorded_tweets_file,"\n")
  ListenForTweets(listenForTerms=terms, streamtime = stream_time, filename=recorded_tweets_file, verbose=TRUE)
  cat("\n\nDone streaming.\nTweets saved: ",recorded_tweets_file,"\n")
}

## Open the saved tweet stream
tweetset_raw <- parse_stream(recorded_tweets_file)
cat("\nStream opened: ",recorded_tweets_file,"\n")

################################
##  Tweetset Post-Processing  ##
################################

## Remove irrelevant columns
tweetset <- tweetset_raw[,keep_columns]



########################################################################################################################



###########################
##  Check the Watchlist  ##
###########################

## Get watchlist-matching tweets
tweetset_watchlist <- CheckWatchlist(tweetset,watchlist)  # null if no matches
print(tweetset_watchlist) # print matches (if any)
## count num watchlist matches
wms <- nrow(tweetset_watchlist) 


#### ---- Data sets needed ---- ####
# - Twitter accounts
## -- exchange accounts
## -- coin official accounts + dev team accounts
## -- crypto personality accounts
## -- bank/govt accounts
## -- technology company accounts
## ...





















########################
##  Review Tweet Set  ##
########################

## Sort by follower count
tweetset_sorted <- arrange(tweetset,desc(followers_count))

## Group by 'is a retweet'    
#    v   v   v   v   v   v   v   v   v   ***  DOESN'T ACTUALLY FILTER THEM OUT !!  ***   v   v   v   v   v   v   v   v   v
tweetset_noRTs_sorted <- tweetset_sorted %>% group_by(is_retweet)
tweetset_noRTs_sorted <- tweetset_noRTs_sorted %>% arrange(desc(followers_count), .by_group = TRUE)

## Print the tweets from the most-followed accounts
print(head(tweetset_noRTs_sorted)[,c(1,2,3,4,5,8)])
Sys.sleep(3)



##############################
##   Search Terms Analysis  ##
##############################

## Print # of tweets collected
cat("\n# Tweets collected:");Sys.sleep(1);cat("\t",nrow(tweetset));Sys.sleep(1);cat("\n\n")

## ... (/by Term)
## ...  - Which terms were detected?
## ...     - Frequency?
## ...

## ... (/by Tweet)
## ...  - Multiple per tweet?
## ...

## ... (more)
## ...
## ...




########################
##  Cashtag Analysis  ##
########################

## (Need ticker references from CMC)
#tkrs <- DownloadCMCData(500)  #  download only if unavailable
tkrs500 <- as.character(tkrs$symb)

## Extract from tweets all symbols that might be cashtags
possible_cashtags <- unlist(tweetset$symbols[!is.na(tweetset$symbols)])

## Verify each symbol is really a cashtag (=is a valid coin symbol and at least in the top 500 mcap)
cashtags_used <- cashtags_used_unique <- c()
#cashtags_used_unique <- c()
for (poss_ct in 1:length(possible_cashtags)) {
  if (possible_cashtags[poss_ct] %in% tkrs500) {
    cashtags_used[length(cashtags_used)+1] <- possible_cashtags[poss_ct]
    if (possible_cashtags[poss_ct] %in% cashtags_used_unique) {
      # already had this coin
    } else {
      cashtags_used_unique[length(cashtags_used_unique)+1] <- possible_cashtags[poss_ct]
    }
  }
}

## Print all cashtags that are used
cat("\n\nFrequency of cashtag use:\n");Sys.sleep(1);print(cashtags_used);Sys.sleep(1)
## Print uniques only
cat("\n\nUnique cashtags used:\n");Sys.sleep(1);print(cashtags_used_unique);Sys.sleep(1)



########################
##  HASHTAG Analysis  ##
########################

## (Need ticker references from CMC)
#tkrs <- DownloadCMCData(500)  #  download only if unavailable
names500 <- as.character(tkrs$name)

## Extract from tweets all symbols that might be HASHTAGS
possible_hashtags <- unlist(tweetset$hashtags[!is.na(tweetset$hashtags)])

## Verify each HASHTAG is really a coin (=is a valid coin name and at least in the top 500 mcap)
hashtags_used <- hashtags_used_unique <- c()
for (poss_ht in 1:length(possible_hashtags)) {
  if (possible_hashtags[poss_ht] %in% names500) {
    hashtags_used[length(hashtags_used)+1] <- possible_hashtags[poss_ht]
    if (possible_hashtags[poss_ht] %in% hashtags_used_unique) {
      # already had this coin
    } else {
      hashtags_used_unique[length(hashtags_used_unique)+1] <- possible_hashtags[poss_ht]
      # add coin to the _unique list
      cat("Added:",possible_hashtags[poss_ht],"\n")
    }
  }
}

## Print all HASHTAG-COIN pairings that are detected
cat("\n\nFrequency of hashtag-coin pairing-detection success:\n");Sys.sleep(1);print(hashtags_used);Sys.sleep(1)
## Print uniques only
cat("\n\nUnique hashtag-coin pairings detected:\n");Sys.sleep(1);print(hashtags_used_unique);Sys.sleep(1)

# match to coins...
# treat like cashtags...





########################
##       VIEW         ##
########################

if (VIEW_NOW == TRUE) {View(tweetset_noRTs_sorted)}



