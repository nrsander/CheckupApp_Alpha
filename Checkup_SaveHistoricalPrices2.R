#
# Checkup_SaveHistoricalPrices.R
#

# source("Checkup_GetHistoricalPrices.R")



# Master 'Save' function
SavePrices <- function(myHistoricalPricesList) {
  
  # Constants
  LOCAL_STORAGE_FOLDER_NAME <- "LocalStorage"
  LOCAL_PRICES_FOLDER_NAME <- "Prices"
  
  # Read input
  saveList <- myHistoricalPricesList
  
  # Run save function
  file_subpath <- paste0(LOCAL_STORAGE_FOLDER_NAME,"/",LOCAL_PRICES_FOLDER_NAME,"/",sep="")
  SavePricesToStorage(myHistoricalPricesList=saveList,myDirectory=file_subpath)
  
  # Print
  cat("Saved!\n")
  Sys.sleep(0.3)
  cat("   >>  Folder:\t",file_subpath,sep="")
  Sys.sleep(0.3)
  cat("\n\n")
  
  # anything else ... ?
  # (return n/a)
  
}


##===============================================================================================================================================================================================================


#########################
#  Batch-Save Function  #
#########################

SavePricesToStorage <- function(myHistoricalPricesList=HistoOHLCV(),myDirectory="LocalStorage/Prices/") {
  
  # Constants (Filename string)
  file_dataDescriptor <- "Price"
  file_filetype <- ".csv"
  
  # Make a Combined xts of all coins' histo Close prices
  combinedClosePrices <- GetHistoClosesCombined(myHistoricalPricesList)
  
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
  
  # Get a vector of all full .csv paths
  allPaths <- c()
  for (paCoin in 1:length(List_AllHistoClosePrices)) {
    iFullPath <- paste0(myDirectory,coinTickers[paCoin],"_",file_dataDescriptor,file_filetype,sep="") # full path to paCoin's price-saves folder
    allPaths[paCoin] <- iFullPath
  }

  # Determine if uCoin's .csv already exists
  coin.CSV.exists <- c()
  for (uCoin in 1:length(allPaths)) {
    # Print line prefix
    cat("\t",names(List_AllHistoClosePrices)[[uCoin]],"\t") # coin ticker
    if (!file.exists(allPaths[uCoin])) {
      # New .csv
      coin.CSV.exists[uCoin] <- FALSE
      status_new.or.existing <- "** New **"
    } else {
      # Existing .csv
      coin.CSV.exists[uCoin] <- TRUE
      status_new.or.existing <- "✓"
    }
    # Print line postfix
    cat(status_new.or.existing,"\n") # coin's .csv status (new/existing)
    Sys.sleep(0.1)
  }
  
  # Print
  cat("\n")
  Sys.sleep(0.5)
  
  
  ##
  ## Either save (new) or update (existing) each coin's .csv
  ##
  
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

      # For each row (time observation) ...
      for (mRow in 1:nrow(mergedcsv)) {
        
        # Print
        cat("\rMerging ...\t",colCoinName,"\t",mRow,"/",nrow(mergedcsv),sep="")
        
        # Collect any warnings
        mWarning <- ""
        
        # Figure out which price data to keep
        if (is.na(mergedcsv[mRow,COLINDEX_NEW]) & is.na(mergedcsv[mRow,COLINDEX_OLD])) {
          # = NA
          mergedcsv[mRow,COLINDEX_MERGED] <- NA
          # ** Warning! **
          mWarning <- paste0("\n\t** Warning **\n\tIn ",colCoinName,", both new and old data have empty values at row ",mRow,".\n\tContinuing...\n",sep="")
          cat(mWarning)
        } else if (!is.na(mergedcsv[mRow,COLINDEX_NEW]) & is.na(mergedcsv[mRow,COLINDEX_OLD])) {
          # = New_
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_NEW]
        } else if (is.na(mergedcsv[mRow,COLINDEX_NEW]) & !is.na(mergedcsv[mRow,COLINDEX_OLD])) {
          # = Old_
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_OLD]
        } else if (!is.na(mergedcsv[mRow,COLINDEX_NEW]) & !is.na(mergedcsv[mRow,COLINDEX_OLD])) { # (ask user: replace? instance or universal?)  ***
          # = New_ (default for conflicting data)
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_NEW]
        } else {
          # = New_ (default for error)
          mergedcsv[mRow,COLINDEX_MERGED] <- mergedcsv[mRow,COLINDEX_NEW]
          # ** Warning! **
          mWarning <- paste0("\n\t** Warning **\n\tIn ",colCoinName,", new data has unhandled complexity at row ",mRow,".\n\tContinuing...\n",sep="")
          cat(mWarning)
        }
        
      }
      
      # Print
      cat("\t100%\n") # Done cycling thru rows
      Sys.sleep(1)
      # Print any warnings collected
      #cat(mWarning)
      
      # Pare the 4 columns down to just 2
      mergedcsv <- mergedcsv[,c(1,4)] 
      #         Index (datetime)    
      #         Close.ABC
      
      # Rename the mergedcsv columns
      names(mergedcsv) <- c("Index",paste0(colOHLC,".",colCoinName,sep=""))
      #names(mergedcsv) <- c("Index",colSuffix)
      
      # Done merging!
      Sys.sleep(0.3)
      
      # Overwrite existing .csv with newly merged data
      cat("\rOverwriting ...\t\t",wCoin,"/",length(coin.CSV.exists),sep="")
      write.csv(as.data.frame(mergedcsv), file=iFilename, na="", row.names = FALSE) # overwrite .csv
      Sys.sleep(0.3)
      
      # Done overwriting!
      
    } # End of "If-else (coin .csv already exists) ... "
    
  } # End of "For (each wCoin) ... "
  
  # Print
  Sys.sleep(0.3)
  cat("\t\t100%\n\n")
  Sys.sleep(0.3)
  
} # End of function


#-------------------------------------------------------------------------------------------------------------------------------------------------------------


# Make 'Combined' xts of all the coins' histo Closes from our custom List
GetHistoClosesCombined <- function(myHistoricalPricesList) {
  
  # Constant
  SUBLIST_INDEX_CLOSE <- 1
  
  # List of coins' histo Close xts vectors
  AllHistoCloses_List <- list()
  for (mhCoin in 1:length(myHistoricalPricesList)) {
    # Print 
    cat("\rExtracting ... \t\t\t",mhCoin,"/",length(myHistoricalPricesList),sep="")
    # Add mhCoin's histo Close xts vector to our list of all coins' histo Closes
    AllHistoCloses_List[[mhCoin]] <- myHistoricalPricesList[[mhCoin]][[SUBLIST_INDEX_CLOSE]]
    Sys.sleep(0.2)
  }
  
  # Copy over element names
  names(AllHistoCloses_List) <- names(myHistoricalPricesList)
  
  # Print
  Sys.sleep(0.2)
  cat("\t✓\n")
  Sys.sleep(0.3)
  
  # Merge list of histo Closes into one all-coin xts of histo Closes
  AllHistoCloses_xts <- do.call(merge, AllHistoCloses_List)
  
  # Print
  cat("\nSuccessful.\n\n")
  Sys.sleep(0.4)
  
  # Return combined xts
  AllHistoCloses_xts
  
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------



##===============================================================================================================================================================================================================
##  TEST RUN
##===============================================================================================================================================================================================================

# Generate test data 
myHistoPrices_List <- HistoOHLCV(InputCoinSet=c("BTC","ETC","STRAT","KMD"),PairCoin="USD",timeframe="minute",pd=1800,myTzone="UTC",showPlot=TRUE)

# Run master save script
SavePrices(myHistoPrices_List)



