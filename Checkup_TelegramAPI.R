
source("Checkup_Dependencies.R")

#API access:
#user_renviron <- path.expand(file.path("~", ".Renviron")); file.edit(user_renviron)


##  Deploys a fully-functional Telegram bot
##        (only works while running)
## 
##       Bot username: #CryptoDunceBot


BOT_NAME <- "DunceBot"

################################################
# Build the Bot
updater <- Updater(token = bot_token(BOT_NAME))
#updater <<- Updater(token = bot_token(BOT_NAME))
dispatcher <- updater$dispatcher
################################################

# --- Event 1 ---
# Define how to handle the start event
start <- function(bot, update) {
  cat("\n","[Msg Rcvd]","\t","Start")
  bot$sendMessage(chat_id = update$message$chat_id, text = "Hello, Mr. Dunce!")
  cat("\t\t","+")
}
# Tell dispatcher how to handle the '/start' command
start_handler <- CommandHandler('start', start)
dispatcher$add_handler(start_handler)


# --- Event 2 ---
# Define how to handle receiving a regular text message
echo <- function(bot, update){
  cat("\n","[Msg Rcvd]","\t",update$message$text,"\n")
  bot$sendMessage(chat_id = update$message$chat_id, text = paste("The phrase '",update$message$text,"' has ",nchar(update$message$text)," characters.",sep=""))
  cat("\t","+",nchar(update$message$text))
}
# Tell dispatcher how to handle a regular text message
echo_handler <- MessageHandler(echo, Filters$text) # What is Filters?
dispatcher$add_handler(echo_handler)


# --- Event 3 ---
# Define how to handle receiving a (command w/ arguments)
caps <- function(bot, update, args){
  cat("\n","[Msg Rcvd]","\t",update$message$text,"\n")
  text_caps <- toupper(paste(args, collapse = ' '))
  bot$sendMessage(chat_id = update$message$chat_id, text = text_caps)
  cat("\t","+",text_caps)
}
# Tell dispatcher how to handle a (command w/ arguments)
caps_handler <- CommandHandler('caps', caps, pass_args = TRUE) # * Note: pass_args = TRUE
dispatcher$add_handler(caps_handler)


#--- Event 4 ----
#  Coins % gains/losses
#----------------
# How to handle?
coins <- function(bot, update, args) {
  
  cat("\n","[Msg Rcvd]","\t",update$message$text,"\n")
  
  # check for no args
  if (is.na(args) || length(args)==0) {
    args<-DownloadCMCData(NumCoins=10)["symb"]
    # NoParamsWarning message
    text_response <- "You didn't specify any coins, so I'll just show you the top 10."
    bot$sendMessage(chat_id = update$message$chat_id, text = text_response)
  }
  
  # Parse for timeframe (first 2 args)
  if (is.numeric(as.numeric(args[1])) && is.character(args[2])) {
    num_ProtectedArguments <- 2
    requested_timeframe <- args[1:num_ProtectedArguments]
    args <- args[(num_ProtectedArguments+1):length(args)] # remove timeframe args
  } else {
    cat("\nUsing default timeframe...\n")
    num_ProtectedArguments <- 0
    requested_timeframe <- c("60","minutes")
    # NoParamsWarning message
    text_response <- paste0("You didn't specify a timeframe, so I'll show you % chg. over the last ",requested_timeframe[1]," ",requested_timeframe[2],"s.")
    bot$sendMessage(chat_id = update$message$chat_id, text = text_response)
  }
  
  # check for no args (AGAIN)
  if (is.na(args) || length(args)==0) {args<-DownloadCMCData(NumCoins=10)["symb"]}
  
  # remove 's' at end of timeframe string
  if (substr(requested_timeframe[2],nchar(requested_timeframe[2]),nchar(requested_timeframe[2])) == "s") {
    requested_timeframe[2] <- substr(requested_timeframe[2],1,nchar(requested_timeframe[2])-1)
  }
  
  # Set vars = cleaned args
  n_Periods <- as.numeric(requested_timeframe[1])
  timeframe <- requested_timeframe[2]
  
  # Find a plot scalar that limits the full chart's API calls
  desired_plot_scalar <- 10
  API_LIMIT <- 2000
  
  # "Plot periods"
  if ((n_Periods*desired_plot_scalar) > API_LIMIT) {
    n_PlotScalar <- round(API_LIMIT/n_Periods,-1)
  } else {
    n_PlotScalar <- desired_plot_scalar
  }
  plot_periods <- n_PlotScalar*n_Periods
  
  
  # Intro msg
  text_response <- paste0("Drawing your ",length(args)," plots ...")
  bot$sendMessage(chat_id = update$message$chat_id, text = text_response)
  
  # Cycle thru each requested coin
  for (iSymb in 1:length(args)) {
    
    # Calculate price change over specified interval
    ohlcv <- HistoOHLCV(InputCoinSet=c(args[iSymb]),PairCoin="USD",timeframe=timeframe,pd=plot_periods,showPlot=TRUE)[[1]][[1]]
    price.current <- as.numeric(ohlcv[nrow(ohlcv)])
    price.n_Periods_ago <- as.numeric(ohlcv[nrow(ohlcv)-n_Periods])
    price.pct_chg <- round(((price.current - price.n_Periods_ago)/price.n_Periods_ago)*100,3)
    cat(args[iSymb],"\t",price.pct_chg,"%\n")

    # Send response (plot as .png)
    dev.copy(png,'plot_from_DunceBot.png')
    dev.off()
    bot$sendPhoto(chat_id = update$message$chat_id, 'plot_from_DunceBot.png', caption = paste0(args[iSymb],"\nPlot: ","Last ",plot_periods," ",timeframe,"s\n","Last ",n_Periods," ",timeframe,"s: ",price.pct_chg,"%"))
  }
  
  # Concluding msg
  text_response <- "Hope that helps!"
  bot$sendMessage(chat_id = update$message$chat_id, text = text_response)
  
}
# Give the handling instructions to the dispatcher
coins_handler <- CommandHandler('coins', coins, pass_args = TRUE) # * Note: pass_args = TRUE
dispatcher$add_handler(coins_handler)




# --- Error: Command Can't Be Handled ---
unknown <- function(bot, update){
  cat("\n","[Msg Rcvd]","\t","[Command Error]")
  cat("\t\t","Cmd causing error:",update$message$text)
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste0("Take your ",update$message$text," and shove it right up your ass!"))
  Sys.sleep(2)
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Sorry about the hostility... I just couldn't understand your command  :(")
}
# Tell dispatcher how to handle a command it can't handle
unknown_handler <- MessageHandler(unknown, Filters$command)
dispatcher$add_handler(unknown_handler)


## --- Kill command ---
#kill <- function(bot, update){
#  # Replace the original line with
#  bot$sendMessage(chat_id = update$message$chat_id,
#                  text = "K bye.")
#  # Clean 'kill' update
#  bot$getUpdates(offset = update$update_id + 1)
#  # Stop the updater polling
#  updater$stop_polling()
#}
## Handle kill command
#kill_handler <- CommandHandler('kill', kill)
#dispatcher$add_handler(kill_handler)






#####################
## Launch DunceBot ##
#####################
updater$start_polling()
#####################















