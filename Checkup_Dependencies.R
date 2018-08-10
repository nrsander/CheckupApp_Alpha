###                           ###
### - * - * - * - * - * - * - ###
### *     C H E C K U P     * ###
### * - * - * - * - * - * - * ###
###                           ###
###   N i c k   S a n d e r   ###
###                           ###
###                           ###
###       DEPENDENCIES        ###
###                           ###


##################
##    Global    ##
##  Parameters  ##
##################

## Index data on top N coins from CMC
n_coins_CMC <- 500
## Timezone
global_timezone <- "UTC"


#################
##    Global   ##
##  Functions  ##
#################

### Enable/Disable warnings
EnableWarnings <- function() {
  if (getOption("warn")==0) {cat("\n * Failed attempt to enable warnings (already enabled)\n")}
  options(warn = 0) # Disable warnings
}
DisableWarnings <- function() {
  if (getOption("warn")==-1) {cat("\n * Failed attempt to disable warnings (already disabled)\n")}
  options(warn = -1) # (Re-)enable warnings
}



################
##   Script   ##
################

# Disable warnings
DisableWarnings()

### Working directory
cat("\nSetting up shop...");Sys.sleep(0.2)
Sys.setenv(TZ = global_timezone)
swd <- "~/Desktop/CRYPTO LYFE/Checkup/CheckupApp"
setwd(swd)
cat("\n\nOkay, let's do this.\n\tWorking directory: \t",swd,"\n\n");Sys.sleep(0.2)

### Libraries:
cat("\nLoading dependencies... (0%)\n");Sys.sleep(0.2)
library(coinmarketcapr) # neat
library(rvest) # read web data
library(jsonlite) # downloading CC API and other internet stuff someday
library(dplyr) # getting our hands dirty with Big Data
library(stringr) # better text
library(anytime) # purportedly great for converting times (e.g. UNIX to DateTime in CCHist)
library(forecast) # forecasting
library(plotrix) # plotting utility
library(tidyr) # clean data is better data
library(ggplot2) # visualization
library(lubridate) # date times
library(scales) # plotting ts
library(gridExtra) # plotting ts
library(ggthemes) # plotting ts
library(zoo) # something about time & dates
library(prophet) # import Facebook's open source Prophet R library
library(quantmod) # download stocks from Yahoo
library(plyr)
library(PerformanceAnalytics)
library(tseries)
library(xts)
library(knitr)
library(magrittr) # %>%
library(telegram)
library(telegram.bot) # telegram bot
library(varhandle)

library(RHmm) # RHmm package is NOT available from CRAN. To install, download .tar.gz from CRAN *archive* (tested using rHmm_2.0.3)      #load.packages('RHmm')


### Common Sources:
source("Checkup_CMCFunctions.R")
source("Checkup_GetHistoricalPrices.R")
source("Checkup_SaveHistoricalPrices.R")
source("Checkup_PortfolioCalculationFunctions.R")
source("Checkup_LoadPortfolios.R")
#source("Checkup_SetOfPortfolioReturns.R") # this is a script...

### For _Regime:
con <- gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb')) # Load Systematic Investor Toolbox  (https://systematicinvestor.wordpress.com/systematic-investor-toolbox/)
source(con)
close(con)

### (Older, unverified sources:)
#-?-source("Checkup_ReadInput.R")
#-?-source("Checkup_PrintReports.R")
#-?-source("Checkup_CCFunctions.R")
#-?-source("Checkup_CCHistFunctions.R")
#-?-source("Checkup_PortfolioFunctions.R")
#-?-source("Checkup_TSGenFunctions.R")
#-?-source("Checkup_TxtReport.R")

### (I'm sure I forgot several new sources, too:)
#-?-source("...")
#-?-source("...")
#-?-source("...")


# Int Helper Constants
#billion = 1000000000
#million = 1000000

# Download CMC data
cmc <- DownloadCMCData(n_coins_CMC)

# Re-enable warnings
EnableWarnings()

# Done!
cat("\nDependencies loaded (100%)\n\n");Sys.sleep(0.2)




