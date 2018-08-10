#####################,
# DBTwitterFunctions #,
#   - Packages        #    Part of DBTwitterScript.R
#   - Functions      #'
#####################'



#################
## Packages:   ##
#################

# (change to "require"?)
library(devtools)
#devtools::install_github("mkearney/rtweet") # need the github version
library(rtweet)
#library(twitteR)



#################
## Functions:  ##
#################

## Connect to twitter API (+ download fresh certificate)
ConnectToTwitter <- function() {

  ## Need a certifcate for permission to use twitter's API
  cat("\nAuthenticating ...\n");Sys.sleep(1)
  download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
  
  ## Authenticate via access token
  cat("Connecting to Twitter ... ");Sys.sleep(1)
  token <- create_token(
    app = "DunceBot",
    consumer_key = "otQJaNDwdPcrQspSAVJQAkKbH",
    consumer_secret = "papdhY0FVHrsJ5QVNtcY2jIRdiNCRwUVYQ4UAddCriVVmWNWnP",
    access_token = "705862841325084672-KPxGwWA6Az9UXbQpBVE99xKeX5Hbj6B",
    access_secret = "95BH8hCD9Yx39fWPaf6vYEqYcL8m54MS4OUkjs2yTAy3Y")
  cat("\t100%");Sys.sleep(0.5);cat("\nConnected.\n");Sys.sleep(0.5) # (doesn't return anything; sets an environmental var)
}


## Create a TweetSet
##    - Get all data on the latest N tweets matching the search criteria
GetTweets <- function(search_terms="Bitcoin",N_tweets=10000,include_retweets=FALSE) {
  
  ## Search for tweets using the search criteria
  cat("\nSearching for the most recent",N_tweets,"tweets about",str_split(search_terms,pattern=",")[[1]][1],"...\n")
  returned_tweets <- search_tweets(search_terms, n=N_tweets, lang="en", include_rts=include_retweets) #tryCatch() **
  
  #
  # <-- lang = en
  #
  
  #
  # <-- FILTER THE SPAM!!!
  #
  
  ## Return entire set of tweets and details (big data frame)
  cat("100%\t\tExtracted",nrow(returned_tweets),"of",N_tweets,"requested tweets.\n")
  returned_tweets
}


## Plot the frequency of tweets over time (needs a tweet_set)
PlotTweetFrequency <- function(tweet_set=GetTweets(),tweets_per="10 minutes",search_term_title="Bitcoin",N_tweets=10000,twitter_timezone="EST") {
  
  ## Plot time series of tweets
  print(ts_plot(tweet_set, tweets_per, tz=twitter_timezone) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
      x = NULL, y = paste0("Tweets of ",search_term_title," per ",tweets_per),
      title = paste0("Frequency of ", search_term_title, " tweets"),
      subtitle = paste0("Tweet counts aggregated within intervals of ", tweets_per,"\n"),
      caption = paste0(nrow(tweet_set)," of ",N_tweets," tweets met signal criteria.  Source: DunceBot @bryptopia")
    ))
  cat("[Plot]\t",search_term_title,"tweet frequency\n")
}


## Listen for tweets (start streaming)
ListenForTweets <- function(listenForTerms, streamtime=75, filename="Test_TweetStream.json", verbose = FALSE) {
  
  ## Print
  if (streamtime==75) {
    cat("\n\tWarning:\tNo streamtime specified, so we're going with",streamtime,"seconds.");Sys.sleep(1)
    cat("\n\tStreaming tweets for",round(75/60,0),"minutes and",mod(75,60),"seconds ...\n\n");Sys.sleep(0.5)
  } else if (streamtime < 60) {
    cat("\n\tWarning:\tStreamtime must be at least 1 minute, \n\t\tso we're going with that.\n\n");Sys.sleep(0.5)
  } else if (streamtime > 60*60) {
      cat("\n\tStreaming tweets for",round(streamtime/60/60,0),"hours,",round(streamtime/60,0),"minutes, and",mod(streamtime,60),"seconds ...\n\n");Sys.sleep(0.5)
  } else if (streamtime > 60) {
      cat("\n\tStreaming tweets for",round(streamtime/60,0),"minutes and",mod(streamtime,60),"seconds ...\n\n");Sys.sleep(0.5)
  }
  
  ## Start streaming tweets
  rt <- stream_tweets(q = listenForTerms, timeout = streamtime, file_name = filename, parse = FALSE, verbose = verbose)
  
  ## no return (file is saved ^)
}


## Get only the tweets from users on the watchlist
CheckWatchlist <- function(tweetset,watchlist) {
  cat("\nChecking the watchlist ...\n\n"); Sys.sleep(0.3)
  
  ## Check the watchlist for these tweets' tweeters
  tweetset$WatchlistMatch <- NA
  for (tw in 1:nrow(tweetset)) {
    if (tweetset$screen_name[tw] %in% watchlist) {
      tweetset$WatchlistMatch[tw] <- TRUE
    } else {
      tweetset$WatchlistMatch[tw] <- FALSE
    }
  }
  
  ## Consider only watchlist-matched tweets  (if none, nrow will =0)
  watchlist_matches <- tweetset %>% 
    filter(WatchlistMatch)
  
  ## If there are any watchlist matches ...
  if (nrow(watchlist_matches) != 0) {
    # Print # of matches
    if (nrow(watchlist_matches)==1) {cat("\n ** ",nrow(watchlist_matches),"watchlist match\t","\n\n")} else {cat(" **   ",nrow(watchlist_matches),"watchlist matches\t","\n\n")}
    # For each matching tweet, print the tweet (and any cashtags detected)
    for (twe in 1:nrow(watchlist_matches)) {
      cat(paste0("\t",twe,")  New tweet from @",watchlist_matches$screen_name[twe],"\t  Retweet:  ",watchlist_matches$is_retweet[twe],"\t  Followers:  ",watchlist_matches$followers_count[twe],"\n\n\t\t\t\t\t  ",watchlist_matches$text[twe],"\n\n"));Sys.sleep(0.3)
      if (!is.na(watchlist_matches$symbols)) {
        for (ctag in 1:length(watchlist_matches$symbols)) {
          cat("\n\t\tCashtag found:\t",watchlist_matches$symbols[[ctag]],"\n")
        }
      }
    }
  } else {
    cat("\n(No watchlist matches)\n")
    watchlist_matches <- NULL
  }
  
  # return tweetset (or null)
  watchlist_matches
}






















## GET TIMELINES:

## get user IDs of accounts followed by CNN
#tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 3200)

## plot the frequency of tweets for each user over time
#tmls %>%
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














################################################


# Locations of tweets:
# lookup_coords()
## (Need Google Maps API key)
## stream tweets from london for 15 seconds
#streamLondon <- stream_tweets(lookup_coords("london, uk"), timeout = 15)

## stream tweets mentioning Trump for a week
#stream_tweets(
#  "realdonaldtrump,trump",
#  timeout = 60 * 60 * 24 * 7,
#  file_name = "tweetsabouttrump.json",
#  parse = FALSE
#)

## read in the data as a tidy tbl data frame
#djt <- parse_stream("tweetsabouttrump.json")




## get user IDs of accounts followed by CNN
#cnn_fds <- get_friends("cnn")

## lookup data on those accounts
#cnn_fds_data <- lookup_users(cnn_fds$user_id)

## get user IDs of accounts following CNN
#cnn_flw <- get_followers("cnn", n = 75000)
## (or do you REALLY want all of them?)
## get them all (this would take a little over 5 days)
#cnn_flw <- get_followers(
#  "cnn", n = cnn$followers_count, retryonratelimit = TRUE
#)

## lookup data on those accounts
#cnn_flw_data <- lookup_users(cnn_flw$user_id)

## search for users with #rstats in their profiles
#usrs <- search_users("#rstats", n = 1000)














################################################


####  SENTIMENT ANALYSIS


## -- Sentiment analysis function #1
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none') {
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. 
  # plyr will handle a list or a vector as an “l” for us
  # we want a simple array (“a”) of scores back, 
  # so we use “l” + “a” + “ply” = “laply”:
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


################################################



SummarizeTweetSet <- function(tweet_set) {
  
  #### Start parsing useful vars
  #### --------------------------
  
  ####  CASHTAGS
  
  ## All CASHTAGS used:
  List_AllCashtagsUsed <- tweet_set$symbols[!is.na(tweet_set$symbols)]
  ## Cashtags used per cashtag-user:
  cashtagsPerCashtagUser <- round(mean(do.call(rbind, lapply(List_AllCashtagsUsed, function(x) length(x)))),2)
  ## Count times each cashtag is mentioned:
  ucoins <- c()
  
  if (length(List_AllCashtagsUsed) > 0) {
    for (twt in 1:length(List_AllCashtagsUsed)) {
      for (ctag in 1:length(List_AllCashtagsUsed[twt])) {
        if(is.character(List_AllCashtagsUsed[[twt]][[ctag]])) {ucoins[length(ucoins)+1] <- toupper(List_AllCashtagsUsed[[twt]][[ctag]])} else {ucoins[length(ucoins)+1] <- List_AllCashtagsUsed[[twt]][[ctag]]}
      }
    }
    freqCashtagTbl <- table(ucoins)
    
    ## Print Results:
    cat("\nFrequency of cashtag mentions:\n");Sys.sleep(1)
    print(freqCashtagTbl);Sys.sleep(2);
    cat("\nSummary:\n\tOf cashtag tweeters...\n\t\t-  Cashtags per tweet:\t",cashtagsPerCashtagUser,"\n\t","xxxxxxxxx 22 xxxxxxxxx\n\t\t-  xxx xxx xxx","\n");Sys.sleep(1)
    
    # ...
    
    ## Print CASHTAG report:
    cat("\n\t# tweets  \t",nrow(tweet_set),"\n");Sys.sleep(1)
    cat("\tUse cashtag\t ",sum(!is.na(tweet_set$symbols))," of ",sum(is.na(tweet_set$symbols)),"\t",round(100*(sum(!is.na(tweet_set$symbols))/sum(is.na(tweet_set$symbols))),1),"%","\n",sep="");Sys.sleep(1)
    #  more ...
    
  }
  
  # ...
  
  cat("\n100%")
  
}







