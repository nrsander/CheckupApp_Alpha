# Dependencies
source("Checkup_DBTwitterFunctions.R")
# Settings
global.retryonratelimit <- TRUE

# Get OAuth'd user's tweets (Glob Env must contain token)
user.N_tweets <- 1000
user.twitter_timezone <- "EST"
baz <- get_timeline("o_hoops", n = user.N_tweets, max_id = NULL, home = TRUE, parse = TRUE, check = TRUE, token = NULL)
minor_slp<-0.4
major_slp<-1

# Plot Frequency
PlotTweetFrequency(baz,
                   tweets_per = "15 minutes",
                   search_term_title = "(Your Timeline)",
                   N_tweets = user.N_tweets,
                   twitter_timezone = user.twitter_timezone)

# Loop print timeline
for (i in rev(1:nrow(baz))) {
  cat("\tTime\t\t\tUsername\n\t",as.character(baz$created_at[i]),"\t",baz$screen_name[i],"\n\n",if(baz$is_retweet[i]==TRUE){"[Retweet - See Original]\n"},if(baz$is_quote[i]==TRUE){"[Quote Tweet - See Original]\n"})      
  Sys.sleep(major_slp)
  cat(baz$text[i])
  Sys.sleep(major_slp)
  cat("\n\n\n\n\n")
  Sys.sleep(minor_slp)
}





table(baz$screen_name)

View(baz)