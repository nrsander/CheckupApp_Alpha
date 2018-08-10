
source("HistoFunctions.R")

# select only the variables you want
getvars <- c("Date", "Type", "TargetCoin", "PairCoin", "Price","Amount","Total")
newdata <- trades.Buys[getvars]

head(newdata)

format <- "%y-%m-%d %H:%M:%S"
newdata$DatetimeEST <- as.POSIXlt(newdata$Date, tz = "EST", format)
newdata$UnixTimestamp <- as.numeric(as.POSIXct(newdata$DatetimeEST))


#- SUBSET RANDOM DATA ---------------------------------------------------
s = 20 # sample size

# ... to only get  *Random* (s)
newdata <- newdata[sample(1:nrow(newdata), s, replace=FALSE),] #  (NOTE:  This is now **UNSORTED**)
head(newdata)
# ... to only get  *First* (s)
#newdata <- newdata[1:s,]
#head(newdata)
#-----------------------------------------------------------------------


# Test another subset out...
getvars <- c("UnixTimestamp","Date","Amount","TargetCoin","Price","PairCoin","Total")
xxx <- newdata[getvars]
names(xxx) <- getvars
head(xxx) # Timestamp and Price only




# Add BTC & ETH prices @ all timestamps...

PairCoinSet <- c("BTC","ETH")

p = length(PairCoinSet)
xxx$BTCUSDThen <- rnorm(s)  ##  random bullshit data
xxx$ETHUSDThen <- rnorm(s)  ##  random bullshit data

for (i in 1:nrow(xxx)) {
  message(round(100*p/s*i,0),"%")
  message("//  Downloading BTC ... ")
  xxx$BTCUSDThen[i] <- GetBTCAtTimestamp(xxx$UnixTimestamp[i])
  message("//  Downloading ETH ... ")
  xxx$ETHUSDThen[i] <- GetETHAtTimestamp(xxx$UnixTimestamp[i])
}

head(xxx)



# USD Values -- At time of trade

xxx$USDValue <- rnorm(s)  ##  random bullshit data

for (i in 1:nrow(xxx)) {
  if (xxx$PairCoin[i] == "BTC") {
    xxx$USDValue[i] = round(xxx$BTCUSDThen[i] * xxx$Total[i],2)
  } else if (xxx$PairCoin[i] == "ETH") {
    xxx$USDValue[i] = round(xxx$ETHUSDThen[i] * xxx$Total[i],2)
  } else {
    message("[   -- ^*^ --   CRITICAL ERROR   -- ^*^ --   ]")
    xxx$USDValue[i] = round(0,2)
  }
}



# USD Values -- Current day

xxx$USDValueCurr <- rnorm(s)  ##  random bullshit data

BTC = 13000

ts = 1515125319 # Jan. X, 2018, like 5th I think;   approaching peak of Early '18 Alt Boom

SecsInDay = 60*60*24

ts = ts+SecsInDay*10

for (i in 1:nrow(xxx)) {
  xxx$USDValueCurr[i] <- xxx$Amount[i] * GetPriceAtTimestamp(InputCoin=xxx$TargetCoin[i],PairCoin="USD",TimestampForPrice=ts)
  message("//  Comparing coins' current values to prices at trade-time ... ",round(100/s*i,0),"%")
}
xxx



message("Average trade USD value        [Set=BUYS; Sample size=",s,"]:    $",round(mean(xxx$USDValue),2))
message("Average current USD value      [Set=BUYS; Sample size=",s,"]:    $",round(mean(xxx$USDValueCurr),2))
message("Average ROI had you HODL'd it  [Set=BUYS; Sample size=",s,"]:   x ",round(mean(xxx$USDValueCurr)/mean(xxx$USDValue),2))
















# -------------------------------------------------------------------------
# sapply  & lapply
# -------------------------------------------------------------------------
# 
# sapply    Works on a LIST or VECTOR of data   Returns a VECTOR
# lapply    "^"                                 Returns a LIST
# 
#Here we will use sapply, which works on a list or vector of data. 
  # #sapply(1:3, function(x) x^2)
  #[1] 1 4 9
# -------------------------------------------------------------------------
#lapply is very similar, however it will return a list rather than a vector:
  #  lapply(1:3, function(x) x^2)
  #[[1]]
  #[1] 1
  #
  #[[2]]
  #[1] 4
  #
  #[[3]]
  #[1] 9
# -------------------------------------------------------------------------
#Passing simplify=FALSE to sapply will also give you a list:
  #  #sapply(1:3, function(x) x^2, simplify=F)
  #[[1]]
  #[1] 1
  #
  #[[2]]
  #[1] 4
  #
  #[[3]]
  #[1] 9
# -------------------------------------------------------------------------
#And you can use unlist with lapply to get a vector.
  #unlist(lapply(1:3, function(x) x^2))
  #[1] 1 4 9
# -------------------------------------------------------------------------




# -------------------------------------------------------------------------
# Exclude columnnds from df's
# -------------------------------------------------------------------------
# exclude variables v1, v2, v3
#myvars <- names(mydata) %in% c("v1", "v2", "v3") 
#newdata <- mydata[!myvars]
# 
# exclude 3rd and 5th variable 
#newdata <- mydata[c(-3,-5)]
#
# delete variables v3 and v5
#mydata$v3 <- mydata$v5 <- NULL
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
# Subset(...) function
# -------------------------------------------------------------------------
# using subset function (part 2)
#newdata <- subset(mydata, sex=="m" & age > 25,
#                  select=weight:income)





# -------------------------------------------------------------------------
# Take a random sample
# -------------------------------------------------------------------------
# take a random sample of size 50 from a dataset mydata 
# sample without replacement
#mysample <- mydata[sample(1:nrow(mydata), 50,
#                          replace=FALSE),]