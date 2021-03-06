###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###           CMC             ###
###          SCRIPT           ###

message("Running CMCScript... (0%)")

##==============================##
###########> Inputs <#############
##==============================##
##                              ##
## ...for CMC Download:         ##
Q_coins <- 300                  ## <-- Download data for the Top __Q__ coins
##                              ##
## ...for Hot Top N Report:     ##
hottopN <- 10                   ## <-- Show the __N__ Hottest coins per timeframe 
searchThruRank <- 100           ## <-- Only search coins ranked in the Top __searchThruRank__
##                              ##  
##################################



## Download CMC Data
CMC <- DownloadCMCData(Q_coins)
## Snapshot of current CMC market & archive it
SnapshotCMC()


## Hot Top N Report (over last 1w, 24h, and 1h)
PrintHotTopNReport(searchThruRank, hottopN, sortReverse=0)
PrintHotTopNReport(searchThruRank, hottopN, sortReverse=1)



#Get.Coin("BTC")  #  this CMC function could be useful ?
message("CMCScript complete.")
message("")

