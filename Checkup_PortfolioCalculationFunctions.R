#source("Checkup_LoadPortfolios.R")  #  added to _Dependencies


###################################
##        Compare Multiple       ##
##          Portfolios           ##
###################################

ComparePortfolios <- function(bags=LoadAllBags(hurry=TRUE),type="cum",PairCoin="USD",timeframe="hour",pd=30*24,k_Lags=1,k_Rolling=30,SavePNG=TRUE,showPlot=FALSE,hurry=TRUE) {
  if (type=="cum") {typeName<-"Cumulative"} else if (type=="rolling") {typeName<-"Rolling"} else {typeName<-"Cumulative(*)"}
  All_Bags_List <- bags
  
  ## Process each portfolio:
  for (i in 1:length(All_Bags_List)) {
    cat("\n\n|  Portfolio ",i,":",sep="");Sys.sleep(0.3);cat(" \t",names(All_Bags_List)[i])
    cat("\n'------------------------------------------------------\n");Sys.sleep(0.1)
    Sys.sleep(0.1)
    if(!hurry){Sys.sleep(2)}else{Sys.sleep(0.3)}
    
    ## Calculate RETURN
    iRet <- Calculate_PortfolioReturn(InputCoinSet=All_Bags_List[[i]][,1],weights=All_Bags_List[[i]][,2],type=type,PairCoin=PairCoin,timeframe=timeframe,pd=pd,k_Lags=k_Lags,k_Rolling=k_Rolling,showPlot=FALSE,hurry=hurry)
    
    DisableWarnings()
    
    ## SMOOTH the return line fx
    smooth.level <- 0.05 # very minor smoothing
    iRet_s <- merge.xts(iRet,c(1:nrow(iRet))) #copy of iRet + an index col
    names(iRet_s) <- c("return","index")
    # (catch smoothing errors)
    tryLoess <- try(loess(return ~ index, data=iRet_s, span=smooth.level))
    tryErrCount <- 0
    while ("try-error" %in% class(tryLoess)) {
      tryErrCount <- tryErrCount+1
      if (tryErrCount<5){
        smooth.level <- smooth.level + 0.02
        tryLoess <- try(loess(return ~ index, data=iRet_s, span=smooth.level))
        cat("\nError:\tLoess span is too small.");Sys.sleep(1);cat("\tResolving...");Sys.sleep(0.5);cat("\nIncreasing Loess span to",smooth.level,"...\n");Sys.sleep(0.5)
      } else {
        cat("\nError:\tToo many attempts to resolve.");Sys.sleep(2);cat("\nThrowing this portfolio in the trash ...");Sys.sleep(2);cat("\n")
        tryLoess <- NULL
      }
    }
    # If there was a smoothing error...
    if (is.null(tryLoess)) {
      iRet_s$smoothed <- 1
    } else {
      iRet_s_loessMod <- loess(return ~ index, data=iRet_s, span=smooth.level) #apply Loess smoothing function
      iRet_s$smoothed <- predict(iRet_s_loessMod)
      if (type=="cum") {cat("Total Return:\t",round(iRet_s$smoothed[length(iRet_s$smoothed)],3),"x\n")} #print total return to date (only for cumulative return calcs)
    }
    EnableWarnings()
    
    ## MERGE each into the master XTS
    if (i == 1) {
      All_Rets <- iRet_s$smoothed
    } else {
      All_Rets <- merge(All_Rets,iRet_s$smoothed)
    }
  }
  names(All_Rets) <- names(All_Bags_List)
  
  ## Remove all rows that contain any NA values
  rowsNA <- nrow(All_Rets)
  All_Rets <- All_Rets[complete.cases(All_Rets),]
  cat("Removed",(rowsNA-nrow(All_Rets)),"of",rowsNA,"periods from portfolio data (contains one or more coins w/ NA data)")
  
  cat("\n\n---\n","\nFinished processing",i,"of",i,"portfolios.","\t (100%)\n") # lazy :(
  Sys.sleep(0.5)
  
  ## SAVE PLOT IMAGE (.png) - Portfolios' returns side-by-side
  if (SavePNG==TRUE) {
    Filename_Saved_PNG <- "All_Portfolios_Comparison.png"
    png(Filename_Saved_PNG,width=1080,height=720)
    plot.xts(All_Rets,legend.loc = "topright",main=paste0("Return (",typeName,")\n\tCustom Indexes"),major.ticks="auto",cex=1.2) # capture for .png
    dev.off()
  }
  
  ## PLOT in console
  #if (showPlot==TRUE) {layout(1);print(plot.xts(All_Rets,legend.loc = "topleft",main=paste0("Return (",typeName,")\n\tCustom Indexes"),cex=0.6))}
  
  ## COMPARE portfolios' returns
  if (type=="cum") {
    All_Final_Rets <- t(round(All_Rets[nrow(All_Rets),],5))
    rownames(All_Final_Rets) <- names(bags)[1:length(All_Final_Rets)]
    colnames(All_Final_Rets) <- "ROI %"
    # print
    cat("\n\nCumulative Returns:  All Portfolios\n-----------------------------------");Sys.sleep(0.5)
    for (j in 1:length(All_Final_Rets)) {
      cat("\n ",paste0(if(All_Final_Rets[j]-1>=0){" "},round((All_Final_Rets[j]-1)*100,2),"%"),"  \t",names(All_Bags_List)[j]);Sys.sleep(0.5)
    }
  }
  if (type=="rolling") {
    cat("\n\nRolling Returns:  All Portfolios\n(Avg. % each",k_Rolling,paste0(timeframe,"s)"),"\n--------------------------------");Sys.sleep(0.5)
    for (j in 1:ncol(All_Rets)) {
      cat("\n ",paste0(round(mean(All_Rets[,j])*100,3),"%"),"  \t",names(All_Bags_List)[j]);Sys.sleep(0.5)
    }
  }
  
  
  ## ...
  
  
  ## Print Start & End times
  cat("\n\nTime Range:\n-----------------------------------\n")
  Print_StartEnd(All_Rets);Sys.sleep(0.5)
  cat("\n")
  
  ## Plot
  if (showPlot) {layout(1);print(plot.xts(All_Rets,legend.loc = "topright", main=paste0("Returns\n\tCustom Indexes"),major.ticks = "auto"))}
  
  ## Return
  All_Rets
}






##===============================================================================================================================================================================================================





###################################
##      Construct Portfolio      ##
##     on the fly  (no .csv)     ##
###################################

# Construct a weighted portfolio   ## ADD PAIR COIN ****
MakePortfolio <- function(InputCoinSet=c("BTC","ETH","BCH","LTC"),weights=0,type="cum",k_Lags=1,k_Rolling=24,title="My Crypto Bags",tf="hour",pds=30*24,showPlot=TRUE,hurry=TRUE) {
  input_coins <- InputCoinSet
  input_weights <- weights
  input_title <- title
  
  # Calculate the (unsmoothed) weighted portfolio return
  wpr <- Calculate_PortfolioReturn(InputCoinSet = input_coins, weights = input_weights, type = type, PairCoin = "USD", timeframe = tf, pd = pds, k_Lags = k_Lags, k_Rolling = k_Rolling)
  Sys.sleep(0.2)
  
  ## SMOOTH the return line fx
  smooth.level <- 0.03 # very minor smoothing
  rez <- merge.xts(wpr,c(1:nrow(wpr))) #copy of wpr + an index col
  names(rez) <- c("return","index")
  # (catch smoothing errors)
  tryLoess <- try(loess(return ~ index, data=rez, span=smooth.level))
  while ("try-error" %in% class(tryLoess)) {
    smooth.level <- smooth.level + 0.02
    tryLoess <- try(loess(return ~ index, data=rez, span=smooth.level))
    cat("\nError:\tLoess span is too small.");Sys.sleep(1);cat("\tResolving...");Sys.sleep(0.5);cat("\nIncreasing Loess span to",smooth.level,"...");Sys.sleep(0.5)
  }
  rez_loessMod <- loess(return ~ index, data=rez, span=smooth.level) #apply Loess smoothing function
  rez$smoothed <- predict(rez_loessMod)
  if (type=="cum") {cat("Total Return:\t",round(rez$smoothed[length(rez$smoothed)],3),"x\n")} #print total return to date (only for cumulative return calcs)
  
  wpr_smooth <- rez$smoothed
  #wpr_smooth[1] <- 1
  
  
  # --- Smoothing ---   <---   UPDATE THE SMOOTHING PROTOCOL !!!
  #copy of wpr
  #rez <- wpr
  #rez <- xts(data.frame(rez,c(1:nrow(rez))),order.by = index(rez)); names(rez) <- c("return","index")
  #smoothing functions
  #rez_loessMod <- loess(return ~ index, data=rez, span=0.03) #  3.0% smoothing span
  #rez$smoothed <- predict(rez_loessMod)
  #wpr_smooth <- rez$smoothed
  #odd correction needed
  #wpr_smooth[1] <- 1
  # -----------------
  
  
  # Plot smoothed portfolio return
  if (showPlot==TRUE) {
    print(plot.xts(wpr_smooth, order_by=index(rez), cex = .4, main=paste0(input_title,"\n\tROI")))
    if (!hurry) {Sys.sleep(2)} else {Sys.sleep(0.5)}
  }
  
  # Print
  #cat("\nTotal return to date:\t",round(wpr_smooth[length(wpr_smooth)],3),"x\n")
  
  # Return
  wpr_smooth
}





##===============================================================================================================================================================================================================


######################################
##      Calculate a Portfolio's     ##
##         Weighted Returns         ##
######################################

# Calculate a portfolio's (weighted) return
Calculate_PortfolioReturn <- function(InputCoinSet=c("BTC","ETH","BCH","LTC"), weights = c(0.65,0.20,0.075,0.075), type="cum", PairCoin = "USD", timeframe = "day", pd = 30*24, k_Lags=1, k_Rolling=round(pd/20,0), title="Returns (Custom Indices)", showPlot=FALSE, returnPortfolioOnly=TRUE, hurry=TRUE) {
  retType <- type
  
  ##   Validate Inputs  
  #########################
  
  ## Type of Return:
  acceptableTypes <- c("cum","rolling")
  if (retType %in% acceptableTypes) {
    if (retType=="rolling") {typeName<-"Rolling"} else {typeName<-"Cumulative"}
    cat("\nCalc Type: ", typeName)
    cat("\n------------------------------\n")
    Sys.sleep(1)
  } else {
    cat("\n\n")
    cat("\n|  Error:\tBAD TYPE!\n|  \tMust use 'cum' or 'rolling'\n|  \n|  Continuing assuming you meant 'cum' ...")
    cat("\n'-----------------------------\n")
    retType <- "cum"
    Sys.sleep(3)
  }
  
  
  ## (auto-calc k_Rolling)
  ## ...
  
  
  ## ...
  ## ... other validation ...
  ## ...
  
  
  ##  Calculate           
  ##  Returns  
  ##########################
  
  # Download historical close prices
  my.coins.prices <- GetHistoClosesCombined(HistoOHLCV(InputCoinSet=InputCoinSet,PairCoin=PairCoin,timeframe=timeframe,pd=pd,hurry=hurry))
  
  # Which calcultion type?    (cum / rolling)
  if (retType == "cum") {
    
    # Calculate CUMULATIVE returns
    my.rets <- Calculate_CumReturns(coinsPrices=my.coins.prices)
    # Plot
    if (showPlot) {cat("\n[Plot]  ",typeName,"returns by coin\n");print(plot(my.rets, legend.loc = c("topleft"), y.intersp = 0, lty = 1, bty = "n", cex = .4, main=paste0(typeName," Returns"), auto.legend = T)); Sys.sleep(1)}
    if(!hurry){Sys.sleep(0.5)}else{Sys.sleep(0.1)}
  } else if (retType == "rolling") {
    
    # Calculate ROLLING returns
    useLogRets <- FALSE # Log scale?    *** <------
    my.rets <- Calculate_RollingReturns(coinsPrices=my.coins.prices,k_Lags=k_Lags,k_Rolling=k_Rolling,log_returns=useLogRets)
    # Plot
    if (showPlot) {cat("\n[Plot]  ",typeName,"returns by coin\n");print(plot(my.rets, legend.loc = c("topleft"), y.intersp = 0, lty = 1, bty = "n", cex = .4, main=paste0(typeName," Returns"), auto.legend = T));Sys.sleep(1)}
    if(!hurry){Sys.sleep(0.5)}else{Sys.sleep(0.1)}
  }
  
  # Get returns to-date (full duration)
  my.rets.toDate <- round(my.rets[nrow(my.rets),],4)
  
  
  ##  Calculate Weighted  
  ##  Portfolio Return    
  ##########################
  
  ## Use default or custom weights?
  if (weights==0) {
    cat("Weights: Equal")
    defaultWeighted <- TRUE
    weights <- rep(1/ncol(my.rets),ncol(my.rets))
  } else {
    if (length(weights) != length(InputCoinSet)) {
      cat("\nError:\tIncorrect number of weights\n\t# coins =",length(InputCoinSet),",","# weights =",length(weights),"\n\tContinuing using default weighting instead (all equal) ...")
      defaultWeighted <- TRUE
      weights <- rep(1/ncol(my.cum.rets),ncol(my.rets))
    } else {
      cat("Weights: Custom")
      defaultWeighted <- FALSE
    }
  }
  cat("\n\n")
  
  ## Apply weights then sum
  portfolio.ret <- rowSums((my.rets * weights))
  portfolio.ret <- xts(portfolio.ret,order.by=index(my.rets))
  
  ## Plot weighted portfolio return
  if (showPlot) {cat("\n[Plot]  ",if(!defaultWeighted){"Weighted"}else{"Unweighted"},typeName,"Portfolio Return\n",sep=""); print(plot(portfolio.ret, y.intersp = 0, lty = 1, bty = "n", cex = .5, main=paste0(title,"\n\tROI"), auto.legend = T))}
  
  ## Return
  if (returnPortfolioOnly==TRUE) {portfolio.ret} else {my.rets}
  
}





##===============================================================================================================================================================================================================



########################################
##       Functions to Calculate       ##
##         Several Variations         ##
##            of 'Returns'            ##
########################################


# Cumulative returns since T(t)
Calculate_CumReturns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),showPlot=FALSE,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  
  combinedCumRets <- coinsPrices
  # * * *
  for (col in 1:ncol(coinsPrices)) {
    firstPr <- as.numeric(coinsPrices[1,col])
    for (row in 2:nrow(coinsPrices)) {
      thisPr <- as.numeric(coinsPrices[row,col])
      combinedCumRets[row,col] <- 1-(firstPr-thisPr)/firstPr
    }
  }
  combinedCumRets[1,] <- 1     ####     DANGEROUS?
  
  # Plot
  if(showPlot){print(plot(combinedCumRets))}
  # Return (or just print)
  if (return==TRUE) {combinedCumRets} else {print(tail(combinedCumRets))}
}

# Returns (periodic)
Calculate_Returns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),k_Lags=1,showPlot=FALSE,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedRets <- apply(coinsPrices, 2, Delt, k=k_Lags)
  rownames(combinedRets) <- as.character(as.POSIXct(index(coinsPrices), origin="1970-01-01"),usetz=F)
  combinedRets <- as.xts(combinedRets[(1+k_Lags):nrow(combinedRets),]) # remove NA rows induced by lag; convert to xts
  # Remove all rows that contain any NA values
  zrows <- nrow(combinedRets)
  combinedRets <- combinedRets[complete.cases(combinedRets),]
  cat("Removed",(zrows-nrow(combinedRets)),"of",zrows,"rows of observations (NA values; mistimed requests?)\n")
  # Plot
  if (showPlot) {cat("Periodic returns\n");print(plot(combinedRets));Sys.sleep(1)}
  # Return (or just print)
  if (return==TRUE) {combinedRets} else {print(tail(combinedRets))}
}

# Log returns
Calculate_LogReturns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),k_Lags=1,showPlot=FALSE,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedLogRets <- diff(log(coinsPrices),lag=k_Lags)
  # Remove all rows that contain any NA values
  zrows <- nrow(combinedLogRets)
  combinedLogRets <- combinedLogRets[complete.cases(combinedLogRets),]
  cat("Removed",(zrows-nrow(combinedLogRets)),"of",zrows,"periods of data (Log-conversion loss)\n")
  # Plot
  if (showPlot == TRUE) {
    cat("Log returns\n")
    print(plot(combinedLogRets))
    Sys.sleep(1)
  }
  # Return (or just print)
  if (return == TRUE) {
    combinedLogRets
  } else {
    print(tail(combinedLogRets))
  }
}

# Rolling returns
Calculate_RollingReturns <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),k_Lags=1,k_Rolling=30,log_returns=TRUE,showPlot=FALSE,return=TRUE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  
  # use log returns or regular returns?
  if (log_returns==TRUE) {
    combinedRets_userchoice <- Calculate_LogReturns(coinsPrices=coinsPrices, k_Lags=k_Lags, showPlot=FALSE, return=TRUE)
  } else {
    combinedRets_userchoice <- Calculate_Returns(coinsPrices=coinsPrices, k_Lags=k_Lags, showPlot=FALSE, return=TRUE)
  }
  
  # calculate rolling returns
  combinedRolRets <- rollapply(combinedRets_userchoice, k_Rolling, sd, fill=NA, align='right')
  
  # remove all rows that contain any NA values
  zrows <- nrow(combinedRolRets)
  combinedRolRets <- combinedRolRets[complete.cases(combinedRolRets),]
  cat("Removed",(zrows-nrow(combinedRolRets)),"of",zrows,"rows of observations (k lags)\n")
  
  # plot rolling returns
  if (showPlot) {cat("Rolling returns\n");print(plot(combinedRolRets));Sys.sleep(1)}
  
  # return (or just print)
  if (return==TRUE) {combinedRolRets} else {print(tail(combinedRolRets))}
  
}


# More advanced calc. functions:
# -------------------------------

# Rolling beta
Calculate_BetaRolling <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),selectCoins=c(2:ncol(coinsPrices),1),k_Lags=1,k_Rolling=30,return=FALSE) {
  coinsPrices <- RemoveCloseFromColNames(coinsPrices)
  combinedLogRets <- Calculate_LogReturns(coinsPrices=coinsPrices,k_Lags=k_Lags,showPlot=FALSE,return=TRUE)
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

#####################
##     HELPER      ##
##    FUNCTIONS    ##
#####################

Print_StartEnd <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV()),return=FALSE) {
  #cat("  Now:\t",as.character(as.POSIXct(Sys.time(), origin="1970-01-01"),usetz=T),"\n\nData:\n-----\n",sep="")
  combined_StartDatetime <- index(coinsPrices)[1]
  combined_EndDatetime <- index(coinsPrices)[nrow(coinsPrices)]
  cat("START:\t",as.character(as.POSIXct(combined_StartDatetime, origin="1970-01-01"),usetz=T),"\n",sep="")
  cat("  END:\t",as.character(as.POSIXct(combined_EndDatetime, origin="1970-01-01"),usetz=T),"\n",sep="")
  if (return==TRUE) {c(combined_StartDatetime,combined_EndDatetime)}
}

RemoveCloseFromColNames <- function(coinsPrices=GetHistoClosesCombined(HistoOHLCV())) {
  myNames <- str_split(colnames(coinsPrices),"\\.")
  for (i in 1:length(myNames)) {
    if (length(myNames[[i]]) > 1) {
      colnames(coinsPrices)[i] <- myNames[[i]][[2]]
    } else {
      colnames(coinsPrices)[i] <- myNames[[i]][[1]]
    }
  }
  coinsPrices
}







##===============================================================================================================================================================================================================




## (Garbo) ##

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










