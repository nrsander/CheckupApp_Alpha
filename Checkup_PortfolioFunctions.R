###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###        PORTFOLIO          ###
###        FUNCTIONS          ###


# -----------------------------------------------------------------
# FUNCTIONS:
# -----------------------------------------------------------------
#  *  ConstructPortfolio(myBag, myQtys)
#       Returns a constructed portfolio with some calculated columns
#
#  *  GetPortfolioValue(myPortfolio, denomination)
#       Returns numerical value
# -----------------------------------------------------------------

ConstructPortfolio <- function(myBag,myQtys) {
  message("Constructing Portfolio... (0%)")
  ## Add Coin and Qty columns
  folio <- data.frame(myBag,myQtys)
  names(folio) <- c("Coin","Qty")
  ## Add CurrPrice column
  i = 0
  for (i in 1:length(myBag)) {
    c <- Get.Coin(myBag[i])
    folio$CurrPrice[i] <- round(c$price,2)
  }
  
  # If (denomination == USD) .......
  
  ## Add USDValue column
  folio$USDValue <- round((folio$Qty * folio$CurrPrice),2)
  ## Add BTCValue column
  i = 0
  for (i in 1:length(myBag)) {
    btc <- Get.Coin("BTC")
    folio$BTCValue[i] <- round((folio$USDValue[i]/btc$price),3)
  }
  ## Add Weight column
  folio$Weight <- round(folio$USDValue / sum(folio$USDValue),3)
  folio$Weight
  
  ## Add PctChg_1h column
  i = 0
  for (i in 1:length(myBag)) {
    c <- Get.Coin(myBag[i])
    folio$PctChg_1h[i] <- c$ch1h
  }
  ## Add PctChg_24h column
  i = 0
  for (i in 1:length(myBag)) {
    c <- Get.Coin(myBag[i])
    folio$PctChg_24h[i] <- c$ch24h
  }
  ## Add PctChg_7d column
  #i = 0
  #for (i in 1:length(myBag)) {
  #  c <- Get.Coin(myBag[i])
  #  folio$PctChg_7d[i] <- c$ch7d
  #}
  ## Add TotValChg_1h column
  i = 0
  for (i in 1:length(myBag)) {
    c <- Get.Coin(myBag[i])
    folio$TotValChg_1h[i] <- round(((folio$USDValue[i] * (1 + (c$ch1h*0.01))) - folio$USDValue[i]),2)
  }
  ## Add TotValChg_24h column
  i = 0
  for (i in 1:length(myBag)) {
    c <- Get.Coin(myBag[i])
    folio$TotValChg_24h[i] <- round(((folio$USDValue[i] * (1 + (c$ch24h*0.01))) - folio$USDValue[i]),2)
  }
  ## Add TotValChg_7d column
  #i = 0
  #for (i in 1:length(myBag)) {
  #  c <- Get.Coin(myBag[i])
  #  folio$TotValChg_7d[i] <- round(((folio$USDValue[i] * (1 + (c$ch7d*0.01))) - folio$USDValue[i]),2)
  #}
  
  # ...
  folio <- folio[order(-folio$USDValue),c(1,2,3,7,8,6,4,5,10,9)] # sort by highest $ value owned (desc.) ... cols nicely ordered
  
  message("Portfolio successfully constructed (100%)")
  folio # return
}



GetPortfolioValue <- function(myPortfolio,denomination="USD") {
  pv <- 0
  if (denomination == "USD") {
    pv <- round(sum(myPortfolio$USDValue),2)
  } else if (denomination == "BTC") {
    pv <- round(sum(myPortfolio$BTCValue),4)
  } else {
    message("ERROR!  Bad denomination.")
    message("Crashing...")
  }
  pv
}

