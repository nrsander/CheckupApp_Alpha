
#
library(prophet) # import Facebook's open source Prophet R library
library(quantmod) # download stocks from Yahoo
library(PerformanceAnalytics)
library(tseries)
library(xts)
library(zoo)
library(knitr)
library(dplyr)

# settings
weeklySeasonality <- FALSE # if FALSE, uses daily seasonality instead (weekly is meaningless for stocks)

# Get close price histo data
getSymbols("AAPL", src = "yahoo")
df <- data.frame(ds = index(AAPL),
                 y = as.numeric(AAPL[,'AAPL.Close']))
plot(df$ds, df$y)

# Train & Test sets
df_train <- df[df$ds >= "2012-01-01" & df$ds <= "2016-12-31", ]
df_test <- df[df$ds >= "2017-01-01", ]

#
m <- prophet(df_train, weekly.seasonality=weeklySeasonality, daily.seasonality = !weeklySeasonality)

#
test_periods <- as.numeric(max(df$ds) - as.Date("2017-01-01")) # max ==> today's date (in stocks' case, discounting weekends/holidays)
future <- make_future_dataframe(m, periods = test_periods)

#nrow(future) ## delete <--
forecast <- predict(m, future) # takes a while
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Plot the forecast
plot(m, forecast)

# Plot the breakdown of Trend vs. Yearly components
prophet_plot_components(m, forecast)

# Signal create
signal <- forecast[, c("ds", "yhat")]
signal <- signal[signal$ds >= "2017-01-01", ]
signal['return'] <- as.numeric(Delt(signal$yhat))
signal$signal <- 0
signal$signal[signal$return >= 0] <- 1
signal$signal[signal$return < 0] <- -1
signal$ds <- as.Date(signal$ds) #added by me to fix error encountered in dplyr::left_join

#
df_test$return <- as.numeric(Delt(df_test$y))
df_test <- dplyr::left_join(df_test, signal[, c("ds", "signal")], by = "ds") #used to cause errors
df_test$signal[is.na(df_test$signal)] <- 0
df_test$return[is.na(df_test$return)] <- 0
df_test$strategy_return <- df_test$return * df_test$signal
rownames(df_test) <- df_test$ds
trade_return <- df_test['strategy_return']

#
charts.PerformanceSummary(trade_return)

#
print(paste0("Cumulative Returns -- ", Return.cumulative(trade_return)))

#
print(paste0("Annualized Returns -- ", Return.annualized(trade_return)))

#
print(paste0("Maximum Drawdown -- ", maxDrawdown(trade_return)))

#
print(paste0("Sharpe Ratio -- ", SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")))

#
message("ProphetScript complete. (100%)")
