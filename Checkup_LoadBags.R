###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###           LOAD            ###
###           BAGS            ###

#>-------------------------------<#
#  FUNCTIONS                      #
#>-------------------------------<#
#                                 #
#--> Load_UserInputBags()         #
#       Pulls user input .csvs    #
#                                 #
#                                 #
#--> Init_Example_Bag_List        #
#       (Params:)                 #
#         weighted           = 0  #   <<---   *** 'weighted' will be REMOVED SOON ***
#                                 #            ^ eventually will be a no-param function; its purpose is to init into global environment (for now)
#>-------------------------------<#


  

#  LOAD USER INPUT BAGS 
#-------------------------------------------- #

Load_UserInputBags <- function() {
  
  #| Load all of the files named like "Bag.*.csv" from CheckupApp/[extra_path]
  #| INPUT STRUCTURE:
  #| ================
  #|-->  myListOfBags   =   LIST ... of Data Frames
  #  >      *  List[[i]] = Data frame (i)
  #               Rows:  {# coins in Bag i}
  #               Cols: 2 (always: Coin & CustomWeight)
  #  >      *  N elements in List = (count of .csv's loaded)
  
  message("Loading Bags from user's .csv files... (0%)")
  
  ##-- FILE PATHS --##
  
  # in which folder are the input .csv files (user's Bags) saved?
  extra_path <- "./Inputs/UserBags/"
  # get a list of user's files from the above folder that are of type .csv
  filenames <- list.files(path = extra_path, pattern="Bag.*.csv")
  # name the Bags after their file names (eventually we will strip "bag." and ".csv")
  userBags_bagnames <- filenames
  # how many bags are there?
  userBags_count <- length(filenames)
  # let user know...
  message("# Bags detected: ",userBags_count)
  message("")
  message("Loading Bags...")
  
  
  ##- Read .csv files from the folder
  user_input.bag.list <- list()
  for (i in 1:length(userBags_bagnames)) {
    user_input.bag.list[[i]] <- read.csv(paste0(extra_path,userBags_bagnames[i]),header=FALSE)#,sep="\t")
    names(user_input.bag.list[[i]]) <- c("Coin","CustomWeight")
    message("Read bag:   ",names(user_input.bag.list)[i],"   (100%)")
    print(user_input.bag.list[[i]])
  }
  
  ##- Give the Bags their true names  
  myBagTitles <- c()
  for (i in 1:length(user_input.bag.list)) {
    myBagTitles[i] <- strsplit(filenames[i],"[.]")[[1]][2]
  }
  names(user_input.bag.list) <- myBagTitles
  
  message("Loaded user's Bags. (100%)")
  print(myBags)
  
  ##- Return the final List
  user_input.bag.list
}

Load_UserInputBags()




#
#
#    SHIT I ACCIDENTALLY WROTE THIS FUNCTION TWICE BUT ONLY UPDATED ONE...
#
#
#    WHICH ONE WORKS AND WHICH ONE DOESN'T ?????????????
#
#
#    TRY TO AVOID EXAMPLE BAGS.
#
#
#     v    v    v    v    v    v    v    v    v    v    v    v    v    v    v
#

#  LOAD EXAMPLE BAGS (N=7) ...
#-------------------------------------------- #

#    *********   vv   Eliminate Weighted=0!
Init_Example_Bag_List <- function(weighted=0) {
  
  ## Compose 7 example Bags to imitate user input
  
  # 1
  Bag.Coinbase.Name <- "Bag.Coinbase"
  Bag.Coinbase.Coins <- c("BTC","ETH","LTC","BCH")
  Bag.Coinbase.CustomWeight <- c(0.25,0.25,0.25,0.25)
  # 2
  Bag.PrivacyAnon.Name <- "Bag.PrivacyAnon"
  Bag.PrivacyAnon.Coins <- c("XMR","ZEC","PIVX")
  Bag.PrivacyAnon.CustomWeight <- c(0.34,0.33,0.33)
  # 3
  Bag.AltPlatforms.Name <- "Bag.AltPlatforms"
  Bag.AltPlatforms.Coins <- c("STRAT","LSK","KMD")
  Bag.AltPlatforms.CustomWeight <- c(0.34,0.33,0.33)
  # 4
  Bag.BarryCoins.Name <- "Bag.BarryCoins"
  Bag.BarryCoins.Coins <- c("BTC","ETC","ZEC")
  Bag.BarryCoins.CustomWeight <- c(0.50,0.25,0.25)
  # 5
  Bag.Financial.Name <- "Bag.Financial"
  Bag.Financial.Coins <- c("XRP","XLM")
  Bag.Financial.CustomWeight <- c(0.50,0.50)
  # 6
  Bag.BigERC20.Name <- "Bag.BigERC20"
  Bag.BigERC20.Coins <- c("ZRX","BAT","SNT","GNT","POWR","LINK","FUN","STORM","MANA","BNB")
  Bag.BigERC20.CustomWeight <- c(0.20,0.20,0.20,0.20,0.20)
  # 7
  Bag.InteroperabilityAlliance.Name <- "Bag.InteroperabilityAlliance"
  Bag.InteroperabilityAlliance.Coins <- c("ICX","WAN","AION")
  Bag.InteroperabilityAlliance.CustomWeight <- c(0.34,0.33,0.33)
  
  ###-- Start the Bags by combining the two input component vectors into a data frame
  Bag.Coinbase <- data.frame(Bag.Coinbase.Coins, Bag.Coinbase.CustomWeight)
  Bag.PrivacyAnon <- data.frame(Bag.PrivacyAnon.Coins, Bag.PrivacyAnon.CustomWeight)
  Bag.AltPlatforms <- data.frame(Bag.AltPlatforms.Coins, Bag.AltPlatforms.CustomWeight)
  Bag.BarryCoins <- data.frame(Bag.BarryCoins.Coins, Bag.BarryCoins.CustomWeight)
  Bag.Financial <- data.frame(Bag.Financial.Coins, Bag.Financial.CustomWeight)
  Bag.BigERC20 <- data.frame(Bag.BigERC20.Coins, Bag.BigERC20.CustomWeight)
  Bag.InteroperabilityAlliance <- data.frame(Bag.InteroperabilityAlliance.Coins, Bag.InteroperabilityAlliance.CustomWeight)

  ###-- Simplify the data frames' column names
  names(Bag.Coinbase) <- c("Coin","CustomWeight")
  names(Bag.PrivacyAnon) <- c("Coin","CustomWeight")
  names(Bag.AltPlatforms) <- c("Coin","CustomWeight")
  names(Bag.BarryCoins) <- c("Coin","CustomWeight")
  names(Bag.Financial) <- c("Coin","CustomWeight")
  names(Bag.BigERC20) <- c("Coin","CustomWeight")
  names(Bag.InteroperabilityAlliance) <- c("Coin","CustomWeight")
  
  ###-- Now create a List containing all of the data frames we just made
  list_userbags <- list(Bag.Coinbase,Bag.PrivacyAnon,Bag.AltPlatforms,Bag.BarryCoins,Bag.Financial,Bag.BigERC20,Bag.InteroperabilityAlliance)
  
  ###-- How many Bags did we just compose and add to this List?
  qty_userbags <- length(list_userbags)  #  [df size = 2 x (n coins in bag)]  ## ** verify
  
  ###-- Give proper names (titles) to the Bags (the data frames)
  names(list_userbags) <- c(Bag.Coinbase.Name,Bag.PrivacyAnon.Name,Bag.AltPlatforms.Name,Bag.BarryCoins.Name,Bag.Financial.Name,Bag.BigERC20.Name,Bag.InteroperabilityAlliance.Name)
  
  ###-- And now we have the foundation to create our third and final column, DefaultWeight, to complete the Bag
  print(head(list_userbags))

  
  
  ###-- Add weighting scheme (DefaultWeight column, for now) to the Bags
  
  #    ****   vv   Eliminate Weighting!
  message("Weighted status: ",weighted)
  
  if (weighted == 0) {
    ###-- Set default (equal) weightings for coins within bags
    for (i in 1:qty_userbags) {
      
      # How many coins in Bag i?
      qtyCoinsInThisBag <- nrow(list_userbags[[i]]) 
      # Assign default (equal) weights to Bag i's coins
      DefaultWeight <- rep((1/qtyCoinsInThisBag),qtyCoinsInThisBag)
      
      #    ****   vv   Eliminate Weighting!
      if (sum(DefaultWeight) != 1) {
        message("WARNING:  sum(DefaultWeight) does not equal 1.000!")
      }
      
      # Combine list_userbags[[i]] and defaultWeight --> into a data frame
      list_userbags[[i]] <- data.frame(list_userbags[[i]],DefaultWeight)  # ... then overwrite list_userbags[[i]] with it.
          #Test this method instead...   #   cbind(list_userbags[[i]],defaultWeight)
    }
  } else if (weighted == 1) {
    #    ****   User provided custom weightings. #    ****    MAY NOT BE NECESSARY...   <---  NONE OF THIS 'IF' STUFF AT ALL IS NECESSARY !!
    message("ERROR! Function Under Construction Kurrently.")
  } else {
    message("ERROR! Bad 'weighted' param")
  }
  #=|    ^ Possibly a better implementation of that:
  #=|        Rather than a separate CustomWeight and DefaultWeight column, maybe just one Weight column?
  #=|        Assign to the Weight col in the if() above
  
  # Return List of Bags' returns
  list_userbags
}
#example.Bag.List <- Init_Example_Bag_List()
  


#  ExampleBag Builder function
#-------------------------------------------- #

Debra <- Init_Example_Bag_List()

Init_Example_Bag_List <- function(weighted=0) {
  
  ## For now just make some example Bags to imitate user input.
  Bag.Coinbase.Name <- "Bag.Coinbase"
  Bag.Coinbase.Coins <- c("BTC","ETH","LTC","BCH")
  Bag.Coinbase.CustomWeight <- c(0.25,0.25,0.25,0.25)
  
  Bag.PrivacyAnon.Name <- "Bag.PrivacyAnon"
  Bag.PrivacyAnon.Coins <- c("XMR","ZEC","PIVX")
  Bag.PrivacyAnon.CustomWeight <- c(0.34,0.33,0.33)
  
  Bag.AltPlatforms.Name <- "Bag.AltPlatforms"
  Bag.AltPlatforms.Coins <- c("STRAT","LSK","KMD")
  Bag.AltPlatforms.CustomWeight <- c(0.34,0.33,0.33)
  
  Bag.BarryCoins.Name <- "Bag.BarryCoins"
  Bag.BarryCoins.Coins <- c("BTC","ETC","ZEC")
  Bag.BarryCoins.CustomWeight <- c(0.50,0.25,0.25)
  
  Bag.Financial.Name <- "Bag.Financial"
  Bag.Financial.Coins <- c("XRP","XLM")
  Bag.Financial.CustomWeight <- c(0.50,0.50)
  
  Bag.BigERC20.Name <- "Bag.BigERC20"
  Bag.BigERC20.Coins <- c("NEO","GAS","ICX","WAN","ONT")
  Bag.BigERC20.CustomWeight <- c(0.20,0.20,0.20,0.20,0.20)
  
  Bag.InteroperabilityAlliance.Name <- "Bag.InteroperabilityAlliance"
  Bag.InteroperabilityAlliance.Coins <- c("ICX","WAN","AION")
  Bag.InteroperabilityAlliance.CustomWeight <- c(0.34,0.33,0.33)
  
  ###-- Make all the Bags (data frames) from combining the two input components (vectors) of each Bag.
  ###-- v
  Bag.Coinbase <- data.frame(Bag.Coinbase.Coins, Bag.Coinbase.CustomWeight)
  Bag.PrivacyAnon <- data.frame(Bag.PrivacyAnon.Coins, Bag.PrivacyAnon.CustomWeight)
  Bag.AltPlatforms <- data.frame(Bag.AltPlatforms.Coins, Bag.AltPlatforms.CustomWeight)
  Bag.BarryCoins <- data.frame(Bag.BarryCoins.Coins, Bag.BarryCoins.CustomWeight)
  Bag.Financial <- data.frame(Bag.Financial.Coins, Bag.Financial.CustomWeight)
  Bag.BigERC20 <- data.frame(Bag.BigERC20.Coins, Bag.BigERC20.CustomWeight)
  Bag.InteroperabilityAlliance <- data.frame(Bag.InteroperabilityAlliance.Coins, Bag.InteroperabilityAlliance.CustomWeight)
  ###-- ^
  
  ###-- Simplify bags' column names
  names(Bag.Coinbase) <- c("Coin","CustomWeight")
  names(Bag.PrivacyAnon) <- c("Coin","CustomWeight")
  names(Bag.AltPlatforms) <- c("Coin","CustomWeight")
  names(Bag.BarryCoins) <- c("Coin","CustomWeight")
  names(Bag.Financial) <- c("Coin","CustomWeight")
  names(Bag.BigERC20) <- c("Coin","CustomWeight")
  names(Bag.InteroperabilityAlliance) <- c("Coin","CustomWeight")
  
  ###-- Put all of our user_input bags (df's) into one List (of df's)    [df size = 2 x (n coins in bag)]
  list_userbags <- list(Bag.Coinbase,Bag.PrivacyAnon,Bag.AltPlatforms,Bag.BarryCoins,Bag.Financial,Bag.BigERC20,Bag.InteroperabilityAlliance)
  
  ###-- Give custom names to the Bags themselves
  names(list_userbags) <- c(Bag.Coinbase.Name,Bag.PrivacyAnon.Name,Bag.AltPlatforms.Name,Bag.BarryCoins.Name,Bag.Financial.Name,Bag.BigERC20.Name,Bag.InteroperabilityAlliance.Name)
  
  
  qty_userbags <- length(list_userbags) # Bag count
  
  #    *********   vv   Eliminate Weighting!
  ###-- Add weighting scheme to Bags
  message("Weighted status: ",weighted)
  if (weighted == 0) {
    
    ###-- Set default (equal) weightings for coins within bags
    for (i in 1:qty_userbags) {
      # How many coins in Bag i?
      qtyCoinsInThisBag <- nrow(list_userbags[[i]]) 
      # Assign default (equal) weights to Bag i's coins
      DefaultWeight <- rep((1/qtyCoinsInThisBag),qtyCoinsInThisBag)
      
      #    *********   vv   Eliminate Weighting!
      if (sum(DefaultWeight) != 1) {
        message("WARNING:  sum(DefaultWeight) does not equal 1.000!")
      }
      # Combine list_userbags[[i]] and defaultWeight --> into a data frame
      list_userbags[[i]] <- data.frame(list_userbags[[i]],DefaultWeight)  # ... then overwrite list_userbags[[i]] with it.
      ##-## Test this method instead...
      ##-## cbind(list_userbags[[i]],defaultWeight)
    }
  } else if (weighted == 1) {
    #    *********   User provided custom weightings.
    #    *********   MAY NOT BE NECESSARY... NONE OF THIS 'IF' STUFF AT ALL!
    message("ERROR! Function Under Construction Kurrently.")
  } else {
    message("ERROR! Bad 'weighted' param")
  }
  #==#   ^ Possibly a better implementation of that:
  #====#     Rather than a separate CustomWeight and DefaultWeight column, maybe just one Weight column?
  #====#     Assign to the Weight col in the if() above
  
  # Return List of Bags' returns
  list_userbags
}
#example.Bag.List <- Init_Example_Bag_List()




