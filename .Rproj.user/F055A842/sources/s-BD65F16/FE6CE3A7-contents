###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###          EXPORT           ###
###            BAG            ###
###          RETURNS          ###
###  (should be more generic) ###


Export_BagReturns_CSV <- function(myBagXTS) {
  output_path <- paste0(swd,"/Outputs/UserBagReturns/")
  output_filename <- paste0("BagReturns_Export",".csv")
  write.csv(myBagXTS, paste0(output_path,output_filename),row.names=F)
  message("Export complete. (100%)")
  message("\tSaved to:\t",output_path,output_filename)
}

