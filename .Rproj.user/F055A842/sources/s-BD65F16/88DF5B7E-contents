# depends on:
# source("Checkup_MasterListTest.R")


##  Commit a batch save of coins' prices
##  --> HistoStorage/Prices_1min
SaveIndivPricesToStorage <- function(xtsOfCoinsPrices) {
  
  cat("\nCommitting coin prices to storage ...\n\n")
  
  ##
  ## Add the same file descriptors
  ## as those in the Returns function.
  ##
  
  #xts.save.prices <- myMasterList[[3]]
  xts.save.prices <- xtsOfCoinsPrices

  # (A) Determine which coins' PRICES to save
  List_CoinPricesForSaving <- list()
  for (i in 1:ncol(xts.save.prices)) {
    ###### If coin.csv doesn't exist...    vvvvvvv
    cat("\rProcessing ... \t",i,"/",ncol(xts.save.prices),"\t",sep="")
    #@names(xts.save.prices)[i] <- str_split(names(xts.save.prices)[i],pattern=" ")[[1]][1] # shrink name from 'ABC Returns' to 'ABC'  # but maybe it should just be 'Returns' ...?
    List_CoinPricesForSaving[[i]] <- xts.save.prices[,i]
    row.names(List_CoinPricesForSaving[[i]]) <- index(List_CoinPricesForSaving[[i]])
    Sys.sleep(0.1)
  }
  names(List_CoinPricesForSaving) <- names(xts.save.prices)
  cat("100%\n")
  Sys.sleep(0.7)
  
  
  # Does a .csv exist for this coin already in HistoStorage?
  coin.CSV.exists <- c()
  for (i in 1:ncol(xts.save.prices)) {
    if(file.exists(paste0("HistoStorage/Prices_1min/",names(List_CoinPricesForSaving)[[i]],"_Prices_1min.csv"))) {
      coin.CSV.exists[i] <- TRUE
    } else {
      coin.CSV.exists[i] <- FALSE
      cat("\tAdded new coin:",names(List_CoinPricesForSaving)[[i]],"\n")
    }
  }
  ##message("Do these coins already have .csv ?")
  ##print(head(coin.CSV.exists))
  
  
  
  # (A) Batch-save to .csv files
  for (i in 1:length(List_CoinPricesForSaving)) {
    cat("\rSaving ... \t",i,"/",length(List_CoinPricesForSaving),"\t",sep="")
    write.zoo(List_CoinPricesForSaving[[i]], file = paste0("HistoStorage/Prices_1min/",names(List_CoinPricesForSaving)[i],"_Prices_1min.csv"), na="")
    Sys.sleep(0.1)
  }
  cat("100%\n")
  Sys.sleep(0.7)
  
  
  
  # (B) Determine which coins' PRICES to **append** to
  
  # (B) Batch-**append** to .csv files
  for (i in 1:length(List_CoinPricesForSaving)) {
    cat("\rUpdating ... \t",i,"/",length(List_CoinPricesForSaving),"\t",sep="")
    #write.zoo(List_CoinPricesForSaving[[i]], file = paste0("HistoStorage/",names(List_CoinPricesForSaving)[i],"_1min.csv"), na="")
    Sys.sleep(0.1)
  }
  
  # Complete
  cat("100%\n")
  Sys.sleep(0.5)
  cat("\nComplete.\n\n")
}







##  Commit a batch save of coins' returns
##  --> HistoStorage/Returns_1min
SaveIndivReturnsToStorage <- function(xtsOfReturns) {
  
  file_parentFolder <- "HistoStorage"
  file_dataDescriptor <- "Returns"
  file_intervalDescriptor <- "1min"
  file_filetype <- ".csv"
  
  xts.of.returns <- xtsOfReturns
  
  cat("Committing returns to storage ...\n\n")
  
  # (A) Determine which coins' returns to save
  
  # empty list
  List_CoinReturnsForSaving <- list() # misnomer ***
  
  # for each coin...
  #    simplify the col/element name from 'ABC Returns' to 'ABC'
  #    decompose xts into individual coin xts obejcts, then add them to the list
  #    set xts row names = datetime of observation
  for (i in 1:ncol(xts.of.returns)) {
    cat("\rProcessing ... \t",i,"/",ncol(xts.of.returns),"\t",sep="")
    names(xts.of.returns)[i] <- str_split(names(xts.of.returns)[i],pattern=" ")[[1]][1] # but maybe it should just be 'Returns' ...?
    List_CoinReturnsForSaving[[i]] <- xts.of.returns[,i]
    row.names(List_CoinReturnsForSaving[[i]]) <- index(List_CoinReturnsForSaving[[i]])
    Sys.sleep(0.1)
  }
  # copy over list elements' names
  names(List_CoinReturnsForSaving) <- names(xts.of.returns)
  cat("100%\n")
  Sys.sleep(0.7)
  
  # Does a .csv file already exist for this (coin + dataType + dataInterval)?
  coin.CSV.exists <- c()
  for (i in 1:length(List_CoinReturnsForSaving)) {
    if(file.exists(paste0(file_parentFolder,"/",file_dataDescriptor,"_",file_intervalDescriptor,"/",names(List_CoinReturnsForSaving)[[i]],"_",file_dataDescriptor,"_",file_intervalDescriptor,file_filetype))) {
      coin.CSV.exists[i] <- TRUE
    } else {
      coin.CSV.exists[i] <- FALSE
      cat("\tAdded new coin:",names(List_CoinReturnsForSaving)[[i]],"\n")
    }
  }
  ##message("Does a .csv file already exist for each (coin + dataType + dataInterval)?")
  ##print(head(coin.CSV.exists))
  
  
  
  # (A) Batch-save to .csv files
  for (i in 1:length(List_CoinReturnsForSaving)) {
    cat("\rSaving ... \t",i,"/",length(List_CoinReturnsForSaving),"\t",sep="")
    write.zoo(List_CoinReturnsForSaving[[i]], file = paste0(file_parentFolder,"/",file_dataDescriptor,"_",file_intervalDescriptor,"/",names(List_CoinReturnsForSaving)[i],"_",file_dataDescriptor,"_",file_intervalDescriptor,file_filetype), na="")
    Sys.sleep(0.1)
  }
  cat("100%\n")
  Sys.sleep(0.7)
  
  
  
  # (B) Determine which coins' returns to **append** to
  
  # (B) Batch-**append** to .csv files
  for (i in 1:length(List_CoinReturnsForSaving)) {
    cat("\rUpdating ... \t",i,"/",length(List_CoinReturnsForSaving),"\t",sep="")
    #write.zoo(List_CoinReturnsForSaving[[i]], file = paste0("HistoStorage/",names(List_CoinReturnsForSaving)[i],"_1min.csv"), na="")
    Sys.sleep(0.1)
  }
  
  # Complete
  cat("100%\n")
  Sys.sleep(0.5)
  cat("\nComplete.\n\n")
}











##  Commit a batch save of coins' returns
##  --> HistoStorage/Returns_1min
SaveIndivReturnsToStorage2 <- function(xtsOfReturns) {
  
  file_parentFolder <- "HistoStorage"
  file_dataDescriptor <- "Returns"
  file_intervalDescriptor <- "1min"
  file_filetype <- ".csv"
  
  xts.of.returns <- xtsOfReturns
  
  cat("Committing returns to storage ...\n\n")
  
  # (A) Determine which coins' returns to save
  
  # empty list
  List_CoinReturnsForSaving <- list() # misnomer ***
  
  # for each coin...
  #    simplify the col/element name from 'ABC Returns' to 'ABC'
  #    decompose xts into individual coin xts obejcts, then add them to the list
  #    set xts row names = datetime of observation
  for (i in 1:ncol(xts.of.returns)) {
    cat("\rProcessing ... \t",i,"/",ncol(xts.of.returns),"\t",sep="")
    names(xts.of.returns)[i] <- str_split(names(xts.of.returns)[i],pattern=" ")[[1]][1] # but maybe it should just be 'Returns' ...?
    List_CoinReturnsForSaving[[i]] <- xts.of.returns[,i]
    row.names(List_CoinReturnsForSaving[[i]]) <- index(List_CoinReturnsForSaving[[i]])
    Sys.sleep(0.1)
  }
  # copy over list elements' names
  names(List_CoinReturnsForSaving) <- names(xts.of.returns)
  cat("100%\n")
  Sys.sleep(0.7)
  
  
  # Does a .csv file already exist for this (coin + dataType + dataInterval)?
  coin.CSV.exists <- c()
  for (i in 1:length(List_CoinReturnsForSaving)) {
    if(file.exists(paste0(file_parentFolder,"/",file_dataDescriptor,"_",file_intervalDescriptor,"/",names(List_CoinReturnsForSaving)[[i]],"_",file_dataDescriptor,"_",file_intervalDescriptor,file_filetype))) {
      coin.CSV.exists[i] <- TRUE
    } else {
      coin.CSV.exists[i] <- FALSE
      cat("\tAdded new coin:",names(List_CoinReturnsForSaving)[[i]],"\n")
    }
  }
  #message("Does a .csv file already exist for each (coin + dataType + dataInterval)?")
  #print(head(coin.CSV.exists))
  
  # Either save or append
  for (i in 1:length(coin.CSV.exists)) {
    iFilename <- paste0(file_parentFolder,"/",file_dataDescriptor,"_",file_intervalDescriptor,"/",names(List_CoinReturnsForSaving)[i],"_",file_dataDescriptor,"_",file_intervalDescriptor,file_filetype)
    if (coin.CSV.exists[i] == TRUE) {
      # Append...
      cat("\rMerging ... \t",i,"/",length(List_CoinReturnsForSaving),"\t",sep="")
      # get existing csv
      oldcsv <- read.csv(iFilename,sep=",")
      # make the new and existing sets match structurally
      newdata <- as.data.frame(List_CoinReturnsForSaving[[i]])
      newdata$Index <- as.character(rownames(newdata))
      newdata <- newdata[,c(2,1)]
      rownames(newdata) <- index(newdata)
      # merge them
      oldcsv <- oldcsv %>% full_join(newdata, by=c("Index","Index"))
      # rewrite merged data to csv
      head(oldcsv)
      
      # bad loop
      #for (i in 1:length(oldcsv)) {
      #  for (j in 1:ncol(oldcsv)) {
      #    if (is.na(oldcsv[i,j])) {
      #      # ???
      #    }
      #  }
      #}
      
      # You can't just eliminate the second column you just added...
      # Make a third column and if there are NA's anywhere, use the non-NA value
      # v v v 
      
      oldcsv <- oldcsv[,1:2]
      names(oldcsv)[2] <- str_split(names(oldcsv)[2],pattern="\\.")[[1]][1]
      head(oldcsv)
      
      write.zoo(oldcsv, file = iFilename, na="")
      Sys.sleep(0.1)
    } else {
      # Save New...
      cat("\rSaving ... \t",i,"/",length(List_CoinReturnsForSaving),"\t",sep="")
      write.zoo(List_CoinReturnsForSaving[[i]], file = paste0(file_parentFolder,"/",file_dataDescriptor,"_",file_intervalDescriptor,"/",names(List_CoinReturnsForSaving)[i],"_",file_dataDescriptor,"_",file_intervalDescriptor,file_filetype), na="",sep=",")
      Sys.sleep(0.1)
    }
  }
  cat("100%\n")
  Sys.sleep(0.7)
  
  cat("\nComplete.\n\n")
}





# Save prices to storage
SaveIndivPricesToStorage(myMasterList[[3]])
# Save returns to storage
SaveIndivReturnsToStorage(viz.xts)
Sys.sleep(2)
SaveIndivReturnsToStorage2(viz.xts)


