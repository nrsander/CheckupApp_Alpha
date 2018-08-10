##install.packages("wordcloud") # fails because of gfortran error while installing package 'slam'
##library(tm) # same # needs package 'slam'
library(twitteR)
library(ROAuth)
library(RColorBrewer)
library(base64enc)




#
#
#  TRASH!!!
#
#





# Twitter API -------------

# Set constant URLs:
requestURL <- "https://api.twitter.com/oauth/request_token" # request
accessURL <- "https://api.twitter.com/oauth/access_token" # access
authURL <- "https://api.twitter.com/oauth/authorize" # auth

# API keys
consumerKey <- "HFTWlWv82x9KvRjnD8p9YTuuC"
consumerSecret <- "iHEXO6F07Eg9SR1jQSylqIBiW8PQpb5MAkp9QDNA7DisblyrtL"
accessToken <- "970423830979207168-d3yordKAyOUg4KCiIbckvXqFFWs6Ie0" # 970423830979207168d3yordKAyOUg4KCiIbckvXqFFWs6Ie0
accessTokenSecret <- "WVuOrIuZsvEIGCpXA0JA54hJSMdkTLfoFv9kMbyiCva3E"

# download CURL certificateL 
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# query params
searchString <- "$ZRX"
Q_tweets <- 1000

# send search query
relevantTweets <- searchTwitter(searchString, n=Q_tweets, lang='en')
print(length(relevantTweets))
print(head(relevantTweets))
#?searchTwitter


setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessTokenSecret) 


