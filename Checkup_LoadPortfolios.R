#
# Checkup_LoadPortfolios.R
# (Aka _LoadBags but that was tkaen by an old garbage file but I'm lazy)
#


####################
##   Functions:   ##
####################

## Load all portfolios/bags in the /MyBags folder
LoadAllBags <- function(hurry=FALSE) {
  
  # Constant Titles:
  PORTFOLIOS_SUBFOLDER <- "MyBags/"
  CSV_COL_NAMES <- c("coin", "weight", "portfolio")
  
  # WD Navigation:
  WD_ORIG <- getwd()
  WD_ORIG<- "/Users/nrsander/Desktop/CRYPTO LYFE/Checkup/CheckupApp" #redundant
  
  #####
  
  ## Output
  portfolio.list <- list()
  
  ## Navigate to /MyBags folder
  setwd(paste0(WD_ORIG,"/",PORTFOLIOS_SUBFOLDER))
  
  ## Get portfolio names
  portfolio.names <- list()
  csvFoundFilenames <- str_split(list.files(pattern="*.csv"),".csv")
  for (i in 1:length(csvFoundFilenames)) {portfolio.names[[i]]<-csvFoundFilenames[[i]][1][1]}
  portfolio.names <- unlist(portfolio.names)
  head(portfolio.names)
  
  ##   Helper function:
  #########################
  ## For reading in all .csv files at once  (adds a column for an entry's originating portfolio's title)
  Read_CSV_Helper <- function(filename) {ret <- read.csv(filename); ret$portfolio <- str_split(filename,".csv")[[1]][1]; ret}
  #########################
  
  
  ##
  ##
  ##  [[ Add an XTS of each portfolio to a master XTS ]]
  ##
  ##
  
  
  ## Load all portfolios
  all.portfolios <- ldply(list.files(pattern="*.csv"), Read_CSV_Helper)
  all.portfolios$coin <- as.character(all.portfolios$coin)
  cat("\nPortfolios Loaded:\n")
  if(!hurry){Sys.sleep(0.5)}else{Sys.sleep(0.1)}
  
  ## Read each folio
  for (i in 1:length(unique(all.portfolios$portfolio))) {
    cat("  ",i,"\t",unique(all.portfolios$portfolio)[i],"\n\t",paste0("(",length(unique(all.portfolios$portfolio))," coins)"),"\n")
    if(!hurry){Sys.sleep(0.5)}else{Sys.sleep(0.1)}
    
    iFolio <- subset(all.portfolios, portfolio == unique(all.portfolios$portfolio)[i], select = c(coin, weight, portfolio))
    
    ## De-factor columns
    iFolio$coin <- as.character(iFolio$coin)
    iFolio$weight <- as.numeric(iFolio$weight)
    iFolio$coin <- as.character(iFolio$coin)
    
    ## Print each folio's contents
    for (row in 1:nrow(iFolio)) {
      cat("\t\t",iFolio$coin[row],"\t",paste0(round(iFolio$weight[row],3)*100,"%"),"\t","\n")
      if(!hurry){Sys.sleep(0.2)}else{Sys.sleep(0.1)}
    }
    cat("\n")
    if(!hurry){Sys.sleep(1)}else{Sys.sleep(0.1)}
    
    ## Add this folio to the list of folios
    portfolio.list[[i]] <- iFolio[,1:2]
    names(portfolio.list)[i] <- iFolio$portfolio[1]
  }
  
  ## Navigate back to the home folder
  setwd(paste0(WD_ORIG))
  if(!hurry){Sys.sleep(1)}else{Sys.sleep(0.1)}
  
  ## Return list of all portfolios
  portfolio.list
}


