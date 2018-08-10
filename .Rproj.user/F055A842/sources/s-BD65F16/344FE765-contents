###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###          PROPHET          ###
###          SCRIPT2          ###
###                           ###

##Sys.setenv(TZ = "UTC")



#########################,
#                    |   #
#        Settings:   |   #
#                    V   #
#########################'
#
InputCoin <- "BTC"
PairCoin <- "USD"
#
Timeframe <- "day"
Periods <- 365*4
#
#   ***
# EVEN BETTER... Determine all 3   v v v  from indices by taking input: trainset_pctOfData <- 70%
#   ***
trainset_StartDate <- "2018-01-01" # ~75%
trainset_EndDate <- "2018-03-31" #
testset_StartDate <- "2018-04-01" # ~25%  (1 day after trainset_EndDate)
#   ***
#
Daily_seasonality <- TRUE
Weekly_seasonality <- TRUE # (use FALSE for stocks)
Yearly_seasonality <- TRUE
#
#|===================
traintest_DateRange <- c(trainset_StartDate,trainset_EndDate,testset_StartDate) # ***
Settings_seasonality <- c(Daily_seasonality,Weekly_seasonality,Yearly_seasonality)
#
#########################



## Run ProphetScript2 on Bitcoin
Run_CoinProphet(InputCoin=InputCoin, PairCoin=PairCoin, Timeframe=Timeframe, Periods=Periods, traintest_DateRange=traintest_DateRange, Settings_seasonality=Settings_seasonality)


Run_CoinProphet <- function(InputCoin="BTC",PairCoin="USD",
                       Timeframe="day",Periods=2*365,
                       traintest_DateRange=c(trainset_StartDate,trainset_EndDate,testset_StartDate),
                       Settings_seasonality=c(Daily_seasonality,Weekly_seasonality,Yearly_seasonality)) {
  # Download a coin's OHLCV data over a period of time
  coin.OHLCV <- GetCoinHistoData(InputCoin,PairCoin,Timeframe,Periods)
  print(head(coin.OHLCV))
  print(str(coin.OHLCV))#df
  
  # In our data frame, we will only consider Date/time ('ds') and Close price ('y')
  coin.OHLCV.Close <- data.frame(ds=coin.OHLCV$time,y=coin.OHLCV$close) # df (closing prices)
  print(head(coin.OHLCV.Close))
  print(str(coin.OHLCV.Close))#df
  
  # Make XTS from our DF (for plotting)
  coin_xts <- xts(coin.OHLCV.Close$y, order.by = coin.OHLCV.Close$ds, tzone="EST") # xts (closing prices)
  names(coin_xts) <- c("y")
  print(head(coin_xts))
  print(str(coin_xts))#xts
  # Plot price over time
  print(plot(coin_xts))#plot
  
  # Recreate the DF (for forecasting)
  coin_df <- data.frame(ds=index(coin_xts),y=as.numeric(coin_xts[,"y"]))
  print(head(coin_df))  
  print(str(coin_df))#df
  
  # Train & Test sets
  head(coin_df)
  tail(coin_df)
  df_train <- coin_df[coin_df$ds >= traintest_DateRange[1] & coin_df$ds <= traintest_DateRange[2], ]
  df_test <- coin_df[coin_df$ds >= traintest_DateRange[3], ]
  
  # All XBT (Cboe) close dates of 2018
  Cboe_Close <- data_frame(
    holiday = 'Cboe_Close',
    ds = as.Date(c('2018-01-17', '2018-02-14', '2018-03-14',
                   '2018-04-18', '2018-05-16', '2018-06-13',
                   '2018-07-18', '2018-08-15', '2018-09-19',
                   '2018-10-17', '2018-11-14', '2018-12-19')),
    lower_window = 0,
    upper_window = 0
  )
  holidays <- Cboe_Close
  
  # Create Prophet Model
  m <- prophet(df_train, daily.seasonality = Settings_seasonality[1], weekly.seasonality=Settings_seasonality[2],yearly.seasonality = Settings_seasonality[3], holidays = holidays)
  
  # Define future
  test_periods <- as.numeric(max(as_datetime(coin_df$ds)) - as_datetime(traintest_DateRange[3])) #  datetime instead... result = same tho (test for Timeframe="day")
  ##test_periods <- as.numeric(max(as.Date(coin_df$ds)) - as.Date(traintest_DateRange[3])) # max(...$ds) == today's date (in stocks' case, excluding weekends/holidays)
  future <- make_future_dataframe(m, periods = test_periods)
  
  ## date range debugging:
  #message("input days = ",Periods)
  #message("coin_df ...", nrow(coin_df))
  #message("df_train ... ",nrow(df_train))
  #message("df_test ... ",nrow(df_test))
  #message("test periods ... ",test_periods)
  #message("future ... ",nrow(future))
  
  # Predict future
  forecast <- predict(m, future) # takes a while
  tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  
  # Plot the forecast
  print(plot(m, forecast))
  
  # Plot the breakdown of Trend vs. Yearly / Weekly / Daily components
  prophet_plot_components(m, forecast)

  
  # Add_regressor ?????
  # Fit.Prophet ?????
  ?add_regressor
  
    
  #  ^  End Prophet
  #  ^
  # -----------------------------------------------
  #  v
  #  v  Start quantmod
  
  
  # Signal create
  signal <- forecast[, c("ds", "yhat")]
  signal <- signal[signal$ds >= traintest_DateRange[3], ]
  signal['return'] <- as.numeric(Delt(signal$yhat))
  signal$signal <- 0
  signal$signal[signal$return >= 0] <- 1
  signal$signal[signal$return < 0] <- -1
  signal$ds <- as.Date(signal$ds) #added by me to fix error encountered in dplyr::left_join
  
  # If it's not indexed by Date, make it be. { this removes "hour" and "min" capabilities :( }
  if (!is.Date(df_test$ds)) {
    df_test$ds <- as.Date(df_test$ds)
  }
  
  # Find delt
  df_test$return <- as.numeric(Delt(df_test$y))
  head(df_test)
  
  #
  df_test <- dplyr::left_join(df_test, signal[, c("ds", "signal")], by = "ds") #previously caused errors (fixed^)
  df_test$signal[is.na(df_test$signal)] <- 0
  df_test$return[is.na(df_test$return)] <- 0
  df_test$strategy_return <- df_test$return * df_test$signal
  rownames(df_test) <- df_test$ds
  trade_return <- df_test['strategy_return']
  
  #
  print(head(df_test)) ##
  print(head(trade_return)) ##
  
  
  #  ^  End quantmod
  #  ^
  # -----------------------------------------------
  #  v
  #  v  Start PerformanceAnalytics
  
  
  #
  charts.PerformanceSummary(trade_return)
  
  ## <>
  price_over_time <- df_test['y'] ## < added
  no_action_return <- (price_over_time[nrow(price_over_time),] - price_over_time[1,]) / price_over_time[1,] # return if you had bought and held and did absolutely nothing (simple then-to-now price diff)
  ## <>
  
  #
  nDigits <- 4
  message("")
  message("Annualized Returns:\t ", round(Return.annualized(trade_return),nDigits))
  message("Cumulative Returns:\t ", round(Return.cumulative(trade_return),nDigits))
  message("'No-action' Returns:\t ",round(no_action_return,nDigits),"   ","\t*Benchmark*") ## <>
  message("--------------------\t---------")
  message("Returns vs No-Action:\t ",round((Return.cumulative(trade_return))-no_action_return,nDigits),"\t*True Performance*") ## <>
  message("")
  message("Maximum Drawdown:\t ", round(maxDrawdown(trade_return),nDigits))
  message("Sharpe Ratio:\t\t ", round(SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev"),nDigits))
  
  #
  message("")
  message("ProphetScript complete. (100%)")
  message("")
}




## Run ProphetScript2 on Bitcoin
#Run_CoinProphet(InputCoin, PairCoin, Timeframe, Periods, traintest_DateRange, Settings_seasonality)









# (!!!)
DetermineTrainTestDates <- function(StartDate="2018-01-01",EndDate=today(),weights=c(0.7,0.3)) {
  message(StartDate)
  message(EndDate)
  # days_btwn <- StartDate - EndDate (date)  ** should eventually be datetime, not date
  # train.size <- days_btwn*weights[1]
  # test.size <- days_btwn*weights[2]
  ## add modulus(train.size) to test.size value
  #
  # train.StartDate <- StartDate
  # train.EndDate <- StartDate + train.size
  # test.StartDate <- train.EndDate + 1
  # test.StartDate <- EndDate
  #
  # returnVec <- c(train.StartDate,train.EndDate,test.StartDate,test.EndDate)
  # returnVec
}





###########################################################################
#  How to generate *ANY* xts                                             ||
###########################################################################
## Single coin:
#single.cr <- Get_CoinReturns(c("NEO"))
#str(single.cr)
#head(single.cr)
## Multiple coins:
#str(Coin.Returns)
#head(Coin.Returns)
## User Bags*:
#str(Bag.Returns)
#head(Bag.Returns)
###########################################################################



###########################################################################
#  Get an xts of any STOCK                                               ||
###########################################################################
# Get close price histo data
#getSymbols("AAPL", src = "yahoo")
#str(AAPL)
#df <- data.frame(ds = index(AAPL),
#                 y = as.numeric(AAPL[,'AAPL.Close']))
# Data frame:
###- Date (named "ds")
###- Close price (named "y")
###########################################################################




#?prophet
#prophet(df = NULL, growth = "linear", 
#        changepoints = NULL, n.changepoints = 25, 
#        yearly.seasonality = "auto", weekly.seasonality = "auto", daily.seasonality = "auto",
#        holidays = NULL, 
#        seasonality.prior.scale = 10,
#        holidays.prior.scale = 10, 
#        changepoint.prior.scale = 0.05,
#        mcmc.samples = 0, 
#        interval.width = 0.8, 
#        uncertainty.samples = 1000,
#        fit = TRUE, ...)


#?fit.prophet
#fit.prophet(m, df, ...)
#(Additional arguments passed to the optimizing or sampling functions in Stan)

