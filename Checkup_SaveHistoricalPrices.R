#
# Checkup_SaveHistoricalPrices.R
#

source("Checkup_GetHistoricalPrices.R")


##
## Commit minutely price data to local storage
## Saves new or overwrites existing .csv files
##


# Master 'Save' function
SavePrices <- function(myListOfOHLCVs) {
  
  # Constants
  LOCAL_STORAGE_FOLDER_NAME <- "LocalStorage"
  LOCAL_PRICES_FOLDER_NAME <- "Prices"
  
  # Read input
  saveList <- myListOfOHLCVs
  
  # Run save function
  file_subpath <- paste0(LOCAL_STORAGE_FOLDER_NAME,"/",LOCAL_PRICES_FOLDER_NAME,"/",sep="")
  SavePricesToStorage(myListOfOHLCVs=saveList,myDirectory=file_subpath)
  
  # Print
  cat("Saved!\t")
  Sys.sleep(0.3)
  cat("--->>  Folder:\t",file_subpath,sep="")
  Sys.sleep(0.3)
  cat("\n\n")
  
  # anything else ... ?
  # (return n/a)
  
}


##===============================================================================================================================================================================================================


#########################
#  Batch-Save Function  #
#########################

# Save function gruntwork
SavePricesToStorage <- function(myListOfOHLCVs=HistoOHLCV(timeframe="minute",pd=1.3*24*60),myDirectory="LocalStorage/Prices/") {
  
  # Constants (Filename string)
  file_dataDescriptor <- "Price"
  file_filetype <- ".csv"
  
  # Check for BadSymbError
  if (myListOfOHLCVs == "oopsError") {return("oopsError")}
  
  # Make a Combined xts of all coins' histo Close prices
  combinedClosePrices <- GetHistoClosesCombined(myListOfOHLCVs)
  
  # Get a vector of all the coin tickers
  coinTickers <- c()
  for (eCoin in 1:ncol(combinedClosePrices)) {
    eCoin.name <- names(combinedClosePrices)[eCoin] # Get coin name
    if (substr(eCoin.name,1,6) == "Close.") {eCoin.name <- substr(eCoin.name,7,nchar(eCoin.name))} # Chop off "Close." from col name
    coinTickers[eCoin] <- eCoin.name # Add coin ticker to vector
  }
  
  # Split up the combined xts of Close prices back into to a List of each coin's histo Close prices separately
  List_AllHistoClosePrices <- list()
  for (oCoin in 1:ncol(combinedClosePrices)) {
    cat("\rChecking for existing data ...",sep="")
    List_AllHistoClosePrices[[oCoin]] <- combinedClosePrices[,oCoin] # data = copy of histo Close prices from oCoin's column
    row.names(List_AllHistoClosePrices[[oCoin]]) <- index(List_AllHistoClosePrices[[oCoin]]) # row name = copy of index (datetime, right?) from the same
    Sys.sleep(0.2)
  }
  
  # Copy over element names
  names(List_AllHistoClosePrices) <- coinTickers
  
  # Print
  cat("\n")
  Sys.sleep(0.3)
  
  
  # ----- File paths for .csv's  -----
  
  # Determine if uCoin's .csv already exists
  coin.CSV.exists <- c()
  
  # Get a vector of all full .csv paths
  allPaths <- c()
  for (paCoin in 1:length(List_AllHistoClosePrices)) {
    iFullPath <- paste0(myDirectory,coinTickers[paCoin],"_",file_dataDescriptor,file_filetype,sep="") # full path to paCoin's price-saves folder
    allPaths[paCoin] <- iFullPath
    # Print line prefix
    cat("\t\t",names(List_AllHistoClosePrices)[[paCoin]],"\t") # coin ticker
    if (!file.exists(allPaths[paCoin])) {
      # New .csv
      coin.CSV.exists[paCoin] <- FALSE
      status_new.or.existing <- "* New File *"
    } else {
      # Existing .csv
      coin.CSV.exists[paCoin] <- TRUE
      status_new.or.existing <- "✓ Exists"
    }
    # Print line postfix
    cat(status_new.or.existing,"\n") # coin's .csv status (new/existing)
    Sys.sleep(0.1)
  }

  #for (uCoin in 1:length(allPaths)) {
  #  # Print line prefix
  #  cat("\t\t",names(List_AllHistoClosePrices)[[uCoin]],"\t") # coin ticker
  #  if (!file.exists(allPaths[uCoin])) {
  #    # New .csv
  #    coin.CSV.exists[uCoin] <- FALSE
  #    status_new.or.existing <- "* New File *"
  #  } else {
  #    # Existing .csv
  #    coin.CSV.exists[uCoin] <- TRUE
  #    status_new.or.existing <- "✓ Exists"
  #  }
  #  # Print line postfix
  #  cat(status_new.or.existing,"\n") # coin's .csv status (new/existing)
  #  Sys.sleep(0.1)
  #}
  
  # Print
  cat("\n"); Sys.sleep(0.1)
  
  
  
  ##        Either
  ##    SAVE (new coins) 
  ##          or
  ## UPDATE (existing coins) 
  ##    each coin's .csv
  
  # For each coin...
  for (wCoin in 1:length(coin.CSV.exists)) {
    
    # Get wCoin's full full path (again...)
    iFilename <- allPaths[wCoin]
    
    # Does wCoin's .csv already exists?
    if (coin.CSV.exists[wCoin] != TRUE) {
      
      ##
      ## Create New
      ##
      
      # Print
      cat("\rSaving ...\t",wCoin,"/",length(coin.CSV.exists),"\t",sep="")
      Sys.sleep(0.2)
      
      # Write file
      write.zoo(List_AllHistoClosePrices[[wCoin]], file = iFilename, na="",sep=",")
      
      # Print
      cat("\t100%\n")
      
      coin.CSV.exists[wCoin] <- TRUE # TEST LINE (1/3)
      
    } #else { # TEST LINE (2/3)
    
    if (coin.CSV.exists[wCoin] == TRUE) { # TEST LINE (3/3)
      
      ##
      ## Merge Existing
      ##
      
      # Get existing .csv
      oldcsv <- read.csv(iFilename,sep=",")

      # Get new data
      newdata <- as.data.frame(List_AllHistoClosePrices[[wCoin]])
      
      # type conv
      oldcsv$Index <- as.character(oldcsv$Index)
      
      # Make the new and existing sets match structurally
      
      # (different)
      head(oldcsv)
      head(newdata)
      # match newdata to oldcsv format (* not great)
      newdata$Index <- as.character(rownames(newdata))
      newdata <- newdata[,c(2,1)]
      rownames(newdata) <- 1:nrow(newdata)
      # (identical)
      #head(oldcsv)
      #head(newdata)
      
      # Merge new into old
      mergedcsv <- oldcsv %>% full_join(newdata, by=c("Index","Index"))
      
      # Rewrite merged data to csv
      mergedcsv <- mergedcsv[,1:3]
        ##  ^   ***
        ##  You can't just eliminate the second column you just added...
        ##  Make a third column and if there are NA's anywhere, use the non-NA value
      
      # Print the pre-edit 3col mergedcsv
      #head(mergedcsv)
      
      # Name the mergedcsv columns
      colOHLC <- str_split(names(oldcsv)[2],pattern="\\.")[[1]][1] # extract OHLC type of oldcsv  (should be 'Close')
      colCoinName <- str_split(names(oldcsv)[2],pattern="\\.")[[1]][2] # extract coin name of oldcsv
      colSuffix <- paste0(colCoinName,".",colOHLC,sep="")
      names(mergedcsv) <- c("Index",paste0("Old_",colSuffix,sep=""),paste0("New_",colSuffix,sep=""))
      
      
      #
      # Compare and choose price to keep (new or old value)
      #
      
      # Constant col indices  *** v ***
      COLINDEX_INDEX <- 1
      COLINDEX_MERGED <- 4
      COLINDEX_NEW <- 3 #  ***  I am unsure about these!!
      COLINDEX_OLD <- 2 #  ***  I am unsure about these!!

      # Collect any warnings and errors
      mWarning <- ""
      mInconsistencies <- 0
      
      # For each row (time observation) ...
      for (mRow in 1:nrow(mergedcsv)) {
        
        # Print
        cat("\rMerging ...\t",colCoinName,"\t",mRow,"/",nrow(mergedcsv),sep="")
        
        # Figure out which price data to keep
        if (is.na(mergedcsv[mRow,COLINDEX_NEW]) & is.na(mergedcsv[mRow,COLINDEX_OLD])) {
          # = NA
          mergedcsv[mRow,COLINDEX_MERGED] <- NA
          # ** Warning A **
          mWarning <- paste0("\t Both new and old data have empty values.  (Continuing...)",sep="")
          cat(mWarning)
          Sys.sleep(0.1)
        } else if (!is.na(mergedcsv[mRow,COLINDEX_NEW]) & is.na(mergedcsv[mRow,COLINDEX_OLD])) {
          # = New_
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_NEW]
          mWarning <- ""
        } else if (is.na(mergedcsv[mRow,COLINDEX_NEW]) & !is.na(mergedcsv[mRow,COLINDEX_OLD])) {
          # = Old_
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_OLD]
          mWarning <- ""
        } else if (!is.na(mergedcsv[mRow,COLINDEX_NEW]) & !is.na(mergedcsv[mRow,COLINDEX_OLD])) { # (ask user: replace? instance or universal?)  ***
          # = New_ (default for conflicting data)
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_NEW]
          mWarning <- ""
          # Inconsistent price at time?
          if (mergedcsv[mRow,COLINDEX_NEW] != mergedcsv[mRow,COLINDEX_OLD]) {
            mInconsistencies <- mInconsistencies + 1
            mWarning <- paste0("Inconsistent values at row","[",mRow,"]","  (Continuing...)")
          }
        } else {
          # = New_ (default for error)
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_NEW]
          # ** Warning B **
          mWarning <- paste0("\t New data has unhandled complexity.  Continuing...",sep="")
          cat(mWarning)
          Sys.sleep(0.1)
        }
        
      }
      
      # Print inconsistency count
      if (mInconsistencies != 0) {cat("\n  Inconsistencies:\t",mInconsistencies,sep="");Sys.sleep(0.2)}
      
      # Pare the 4 columns down to just 2 ("Index", "Close.ABC")
      mergedcsv <- mergedcsv[,c(1,4)] 
      
      # Rename the mergedcsv columns
      names(mergedcsv) <- c("Index",paste0(colOHLC,".",colCoinName,sep=""))
      
      # Print
      cat("\n\n")
      Sys.sleep(0.3)
      
      # Done merging!

      # Overwrite existing .csv with newly merged data
      cat("\rOverwriting ...\t\t",wCoin,"/",length(coin.CSV.exists),sep="")
      write.csv(as.data.frame(mergedcsv), file=iFilename, na="", row.names = FALSE) # overwrite .csv
      Sys.sleep(0.3)
      
      # Done overwriting!
      
    } # End of "If-else (coin .csv already exists) ... "
    
  } # End of "For (each wCoin) ... "
  
  # Print
  Sys.sleep(0.2)
  cat("\t\t100%\n\n")
  Sys.sleep(0.2)
  
} # End of function


#-------------------------------------------------------------------------------------------------------------------------------------------------------------



##===============================================================================================================================================================================================================
##  TEST RUN
##===============================================================================================================================================================================================================

#### Generate test data 
###myListOfOHLCVs <- HistoOHLCV(InputCoinSet=c("BTC","ETC","STRAT","KMD"),PairCoin="USD",timeframe="minute",pd=1800,myTzone="UTC",showPlot=TRUE)

#### Run master save script
###SavePrices(myListOfOHLCVs)



