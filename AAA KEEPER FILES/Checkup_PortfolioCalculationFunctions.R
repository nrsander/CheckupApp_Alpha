
#######################################
##  Portfolio Calculation Functions  ##
#######################################

###############
#  FUNCTIONS  #
###############

# Returns (periodic)
Calculate_Returns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),k_Lags=1,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedRets <- apply(coinsPrices, 2, Delt, k=k_Lags)
  rownames(combinedRets) <- as.character(as.POSIXct(index(coinsPrices), origin="1970-01-01"),usetz=F)
  combinedRets <- as.xts(combinedRets[(1+k_Lags):nrow(combinedRets),]) # remove NA rows induced by lag; convert to xts
  # Remove all rows that contain any NA values
  zrows <- nrow(combinedRets)
  combinedRets <- combinedRets[complete.cases(combinedRets),]
  cat("Removed",(zrows-nrow(combinedRets)),"of",zrows,"rows of observations (NA values)\n")
  # Print
  cat("Periodic returns","\n",sep="")
  Sys.sleep(1)
  # Plot
  print(plot(combinedRets))
  # Return (or just print)
  if (return==TRUE) {combinedRets} else {print(tail(combinedRets))}
}

# Log returns
Calculate_LogReturns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),k_Lags=1,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedLogRets <- diff(log(coinsPrices),lag=k_Lags)
  # Remove all rows that contain any NA values
  zrows <- nrow(combinedLogRets)
  combinedLogRets <- combinedLogRets[complete.cases(combinedLogRets),]
  cat("Removed",(zrows-nrow(combinedLogRets)),"of",zrows,"rows of observations (NA values)\n")
  # Plot
  print(plot(combinedLogRets))
  # Return (or just print)
  if (return==TRUE) {combinedLogRets} else {print(tail(combinedLogRets))}
}

# Rolling returns
Calculate_RollingReturns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),k_Lags=1,k_Rolling=30,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedLogRets <- Calculate_LogReturns(coinsPrices,k_Lags,TRUE)
  combinedRolRets <- rollapply(combinedLogRets, k_Rolling, sd, fill=NA, align='right')
  # Remove all rows that contain any NA values
  zrows <- nrow(combinedRolRets)
  combinedRolRets <- combinedRolRets[complete.cases(combinedRolRets),]
  cat("Removed",(zrows-nrow(combinedRolRets)),"of",zrows,"rows of observations (NA values)\n")
  # Plot
  print(plot(combinedRolRets))
  # Return (or just print)
  if (return==TRUE) {combinedRolRets} else {print(tail(combinedRolRets))}
}


# Cumulative returns   # *******************
## combinedCumRets <- sapply(coredata(combinedRets), function(x) cumprod(1 + x) - 1)
## rownames(combinedCumRets) <- as.character(as.POSIXct(index(combinedRets), origin="1970-01-01"),usetz=F)
# Print & plot
## tail(combinedCumRets)
## plot(combinedCumRets)


# -----


# Rolling beta
Calculate_BetaRolling <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),selectCoins=c(2:ncol(coinsPrices),1),k_Lags=1,k_Rolling=30,return=FALSE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedLogRets <- Calculate_LogReturns(coinsPrices,k_Lags,TRUE)
  rollingbeta <- rollapply(zoo(combinedLogRets),
                           width=k_Rolling,
                           FUN = function(Z) 
                           { 
                             t = lm(formula(as.data.frame(Z[,selectCoins])), data = as.data.frame(Z)); 
                             return(t$coef) 
                           },
                           by.column=FALSE, align="right") 
  rollingbeta <- fortify(rollingbeta,melt=TRUE)
  # Remove all rows that contain any NA values
  zrows <- nrow(rollingbeta)
  rollingbeta <- rollingbeta[complete.cases(rollingbeta),]
  cat("Removed",(zrows-nrow(rollingbeta)),"of",zrows,"rows of observations (NA values)\n")
  # Plot
  print(ggplot(rollingbeta) + geom_line(aes(x=Index,y=Value)) + facet_grid(Series~.) + theme_bw())
  # Return (or just print)
  if (return==TRUE) {rollingbeta} else {print(tail(rollingbeta))}
}

# Correlations of betas
Calculate_BetaCorrelations <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),return=FALSE) {
  z <- RemoveCloseFromColNames(coinsPrices)
  z.logrtn <- diff(log(z))
  c <- cor(z.logrtn,use="complete.obs") #  “pairwise.complete” to use periods even if all are not avail.
  # Remove all rows that contain any NA values
  zrows <- nrow(c)
  c <- c[complete.cases(c),]
  cat("Removed",(zrows-nrow(c)),"of",zrows,"rows of observations (NA values)\n")
  # Return (or just print)
  if (return==TRUE) {c} else {print(c)}
}

# Rolling correlations of betas
Calculate_BetaRollingCorrelations <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),selectCoins=c(1:ncol(coinsPrices)),k_Rolling=30,return=FALSE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)[,selectCoins]
  c <- Calculate_BetaCorrelations(coinsPrices,return=TRUE)
  z.logrtn <- diff(log(coinsPrices))
  ut <- upper.tri(c)
  n <- paste(colnames(c)[row(c)[ut]],colnames(c)[col(c)[ut]])
  rollingcorr <- rollapply(z.logrtn,
                           width=k_Rolling,
                           FUN = function(Z)
                           {
                             return(cor(Z,use="pairwise.complete.obs")[ut])
                           },
                           by.column=FALSE, align="right")
  colnames(rollingcorr) <- n
  rollingcorr.df <- fortify(rollingcorr,melt=TRUE)
  # Remove all rows that contain any NA values
  zrows <- nrow(rollingcorr)
  rollingcorr <- rollingcorr[complete.cases(rollingcorr),]
  cat("Removed",(zrows-nrow(rollingcorr)),"of",zrows,"rows of observations (NA values)\n")
  # Plot
  print(ggplot(rollingcorr.df,aes(x=Index)) +
          geom_ribbon(aes(ymin=0,ymax=Value)) +
          facet_grid(Series~.) +
          ylim(c(-1,1)) +
          theme_bw())
  # Return (or just print)
  if (return==TRUE) {rollingcorr} else {print(tail(rollingcorr))}
}


##===============================================================================================================================================================================================================


###############
#   HELPER    #
#  FUNCTIONS  #
###############

Print_StartEnd <- function(inp_xts,return=FALSE) {
  #cat("  Now:\t",as.character(as.POSIXct(Sys.time(), origin="1970-01-01"),usetz=T),"\n\nData:\n-----\n",sep="")
  combined_StartDatetime <- index(inp_xts)[1]
  combined_EndDatetime <- index(inp_xts)[nrow(inp_xts)]
  cat("START:\t",as.character(as.POSIXct(combined_StartDatetime, origin="1970-01-01"),usetz=T),"\n",sep="")
  cat("  END:\t",as.character(as.POSIXct(combined_EndDatetime, origin="1970-01-01"),usetz=T),"\n",sep="")
  if (return==TRUE) {c(combined_StartDatetime,combined_EndDatetime)}
}
#Print_StartEnd(combined_Closes)

RemoveCloseFromColNames <- function(df) {
  myNames <- str_split(colnames(df),"\\.")
  for (i in 1:length(myNames)) {
    if (length(myNames[[i]]) > 1) {
      colnames(df)[i] <- myNames[[i]][[2]]
    } else {
      colnames(df)[i] <- myNames[[i]][[1]]
    }
  }
  df
}
#coinsPricesNoClose <- RemoveCloseFromColNames(coinsPrices)


##===============================================================================================================================================================================================================





###############
#   (Garbo)   #
###############

# Plot the effect of Consensus 2018 on ETH-BTC beta
Plot_Consensus2018 <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),selectCoins=c(1,2),return=FALSE) {
  # Consensus 2018 start & end dates
  StartDate_Consensus2018 <- as.POSIXct(as.Date("2018-05-14"))
  EndDate_Consensus2018 <- as.POSIXct(as.Date("2018-05-16"))
  # Find rolling beta (BTC-ETH)
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  rollingbeta <- Calculate_BetaRolling(coinsPrices,selectCoins=selectCoins,k_Lags=k_Lags,k_Rolling=k_Rolling,return=TRUE)
  rollingbeta.consensus2018 <- window(rollingbeta,start="2018-05-03",end="2018-06-03")[,c(selectCoins[-1])] # Zoom in to 1 month *** (better way?)
  rollingbeta.consensus2018.df <- fortify(rollingbeta.consensus2018,melt=TRUE)
  # Print
  cat("Consensus 2018","\n",sep="")
  Sys.sleep(1)
  # Plot
  library(grid) # for arrows
  print(ggplot(rollingbeta.consensus2018.df,aes(x=Index)) +
          geom_ribbon(aes(ymin=0,ymax=Value)) +
          annotate("text",x=StartDate_Consensus2018,y=2+0.1,label="Start",size=3,hjust=0.5,vjust=0) +
          annotate("text",x=EndDate_Consensus2018,y=2-0.1,label="End",size=3,hjust=0.5,vjust=0) +
          geom_segment(aes(x=StartDate_Consensus2018, y=2, xend=StartDate_Consensus2018, yend=0),colour="red", arrow=arrow(length=unit(0.5, "cm"))) +
          geom_segment(aes(x=EndDate_Consensus2018, y=1.8, xend=EndDate_Consensus2018, yend=0),colour="red", arrow=arrow(length=unit(0.5, "cm"))) +
          theme_bw())
  # Return (or just print)
  if (return==TRUE) {rollingbeta.consensus2018.df}
}





