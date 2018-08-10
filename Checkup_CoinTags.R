
####  Top 100 tickers  ----------

## CMC download
tickers <- DownloadCMCData(400) # download top 400
tickers <- as.character(tickers$symb) # tickers only
top100 <- tickers[1:100] # narrow to top 100






####  Functions  ----------

## Get all tags of a given coin
GetTags <- function(coin) {
  tags <- read.csv(paste0("tags/tags-",coin,".csv"))
  names(tags) <- c("X","tag")
  as.character(tags$tag)
}

### Add tags to a given coin (FILE MUST ALREADY EXIST!!!)
#AddTags <- function(coin,tags) {
#  tags <- read.csv(paste0("tags/tags-",coin,".csv"))
#  # add all new tags
#  for (oTag in 1:length(tags)) {tags[length(tags)+1] <- tags[oTag]}
#  tags$tag <- as.character(tags$tag)
#  # resave
#  write.csv(tags,file=paste0("tags/tags-",coin,".csv"))
#}
#
#AddTags("BTC",c("Top 10"))



#########################
##  Store sample data  ##
#########################
#coin <- "BTC"
#ww <- c("Store of Value","Proof of Work","Coinbase","Fiat Gateway")
#write.csv(ww,file=paste0("tags/tags-",coin,".csv"))

