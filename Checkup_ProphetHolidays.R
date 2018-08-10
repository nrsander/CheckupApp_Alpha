


AddHolidays_CoinProphet <- function() {
  
  
  Cboe_Close <- data_frame(
    holiday = 'Cboe_Close',
    ds = as.Date(c('2018-01-17', '2018-02-14', '2018-03-14',
                   '2018-04-18', '2018-05-16', '2018-06-13',
                   '2018-07-18', '2018-08-15', '2018-09-19',
                   '2018-10-17', '2018-11-14', '2018-12-19')),
    lower_window = 0,
    upper_window = 0
  )
  # all 12 XBT close dates of 2018
  
  Big_Fin_Entrance <- data_frame(
    holiday = 'Big_Fin_Entrance',
    ds = as.Date(c('2017-10-31', 'x', 'x')),
    lower_window = 0,
    upper_window = 1
  )
  # CBOE, 
  
  BTC_Forks <- data_frame(
    holiday = 'BTC_Forks',
    ds = as.Date(c('2017-08-01', '2017-10-25', 'x', 'x')),
    lower_window = 0,
    upper_window = 1
  )
  # BCH, BTG, 
  
  Big_Fin_Disparage <- data_frame(
    holiday = 'Big_Fin_Disparage',
    ds = as.Date(c('2017-09-12', 'x', 'x')),
    lower_window = 0,
    upper_window = 1
  )
  # Jamie Dimon "BTC is a fraud", 
  
  Big_Nation_Ban <- data_frame(
    holiday = 'Big_Nation_Ban',
    ds = as.Date(c('2013-12-05','2017-03-10', '2017-03-28', '2017-09-15', 'x', 'x')),
    lower_window = 0,
    upper_window = 1
  )
  # China bans Big Fin from using BTC (the OG China ban), SEC denies Winklevoss BTC ETF application, SEC denies 2nd BTC ETF application (SolidX Bitcoin Trust), the China Ban, 
  
  Big_New_Psych_Price_Barrier <- data_frame(
    holiday = 'Big_New_Psych_Price_Barrier',
    ds = as.Date(c('2017-01-03', 'c', 'b', 'a', 'x')),
    lower_window = 0,
    upper_window = 1
  )
  # BTC breaks $1000 for first time in 3 years, 
  
  Big_Hacks <- data_frame(
    holiday = 'Big_Hacks',
    ds = as.Date(c('2015-01-04', '2016-08-02', 'c', 'b', 'a', 'x')),
    lower_window = 0,
    upper_window = 1
  )
  # Bitstamp hacked, Bitfinex hacked, 
  
  BTC_Block_Halvings <- data_frame(
    holiday = 'BTC_Block_Halvings',
    ds = as.Date(c('2012-11-28', '2016-07-09', '2020-07-10')),
    lower_window = 0,
    upper_window = 1
  )
  # 1st halving day , 2nd halving day, 3rd halving day {"next in July 2020 (rough)"} 
  
  
  holidays <- bind_rows(playoffs, superbowls)
}

