##################
## Dependencies ##
##################
source("Checkup_DBTwitterFunctions.R")
## Settings:
global.retryonratelimit <- TRUE
##==================================================================



#################
## User inputs ##
#################

# -- Get tweets --
user.search_terms <- "BazCap,@BazCap"     #    one string; separate terms with commas
user.include_retweets <- TRUE    #    
user.N_tweets <- 13000            #    N_tweets rate limit: 18,000 every 15 mins (72,000 per hour)

# -- Plot tweet frequency --
user.tweets_per <- "24 hours"    #    bucketizes tweets; defines 'frequency' as "tweets per (N) (period)s"
user.twitter_timezone <- "UTC"    #    this isn't even working?

# -- Sentiment analysis --
run.sentiment.analysis <- FALSE   #    (not ready for primetime)




##==================================================================


############
## Script ##
############

## Inferred vars
#search_term_title:
if (str_detect(user.search_terms,",")) {search_term_title <- str_split(user.search_terms,pattern=",")[[1]][1]} else {search_term_title <- user.search_terms} # single or multiple keyword inputs? if multiple, use first.

## Connect
ConnectToTwitter()

## Scan for tweets
tweet_set <- GetTweets(search_terms = user.search_terms,
                       N_tweets = user.N_tweets,
                       include_retweets = user.include_retweets
)

## Plot tweet frequency over time
PlotTweetFrequency(tweet_set,
                   tweets_per = user.tweets_per,
                   search_term_title = search_term_title,
                   N_tweets = user.N_tweets,
                   twitter_timezone = user.twitter_timezone
)

#View(tweet_set)








##==================================================================


## ...










## ...


##==================================================================



########################
## Sentiment Analysis ##
########################

if (run.sentiment.analysis == TRUE) {
  # -- Get positive & negative wordsets:
  download.file(url="https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/positive-words.txt",destfile="pos_words.txt")
  download.file(url="https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/negative-words.txt",destfile="neg_words.txt")
  pos_words <- read.csv("pos_words.txt")
  neg_words <- read.csv("neg_words.txt")
  # 1) regular?
  pos_words <- as.character(unlist(pos_words)[48:nrow(pos_words)])
  neg_words <- as.character(unlist(neg_words)[48:nrow(neg_words)])
  # 2) get polarity scores
  scored_words <- read.csv("scoredWords.txt",sep="\t",header = FALSE); names(scored_words) <- c("word","score")
  ##  ***
  # df
  tweet_set_df <- as.data.frame(tweet_set)
  element_options <- names(tweet_set_df)
  ##  ***
  # score
  result <- score.sentiment(tweet_set_df$text,pos_words,neg_words)
  result_meanscore <- mean(result$score)
  # print
  print(summary(result))
  hist(result$score,col ="orange", main =paste0("Twitter Emotion","\n(Sentiment score: -5 to 5)"), ylab = "# of tweets",xlab= paste0("Mean score = ",round(result_meanscore,2)))
  head(result$text)
}


##==================================================================







