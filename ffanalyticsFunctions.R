###################################
#####                         #####
###       FantasyTron 1.0       ###
###         Dependencies        ###
#####                         #####
###################################

#################
##  LIBRARIES  ##
#################

#install.packages(c("devtools","rstudioapi"), dependencies=TRUE, repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
library(devtools)
library(rstudioapi)
#devtools::install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
#devtools::install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
#devtools::install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
#devtools::install_github(repo = "isaactpetersen/ffanalytics") # requires XQuartz; download from https://www.xquartz.org/
library(ffanalytics)
library(dplyr)
library(magrittr)
#install.packages("googlesheets")
library(googlesheets)



#################
##  FUNCTIONS  ##
#################

#- Assemble data into a table by team
AssembleTeamTable <- function(tData) {
  ##  If  this  fails, it's because you loaded plyr AFTER dplyr ... which I have not fixed!
  test.table <- tbl_df(test.final)
  test.table <- group_by(test.table, Owner)
  test.table <- summarise(test.table,
                          TotADPVal = sum(ADPValue),
                          TotFPts = sum(FPts),
                          AvgADP = mean(ADP)
  )
  tData <- test.table
  # Return
  tData
}


#- Scrape GL sheet for all 2018 draft picks owned
PullDraftPicks <- function() {
  
  #- Print
  cat("\nPulling 2018 draft picks from the GL sheet ...\n");Sys.sleep(0.3)
  
  #- Download the GL Sheet
  GL.gsheet.sheet_key <- "1-4OpFBJfmGhHkZ4D3ZrYvRofyXncn8dlpq_nOdF0cGc"
  GL.gsheet <- gs_key(GL.gsheet.sheet_key)
  
  ###  (For which Google Sheets are you authenticated?)
  ###  authedshts <- gs_ls()  
  
  # get Sheet details
  GL.gsheet.title <- GL.gsheet[["sheet_title"]]
  GL.gsheet.url <- GL.gsheet[["browser_url"]]
  GL.gsheet.updated <- GL.gsheet[["updated"]]
  GL.gsheet.updated_mins_ago <- as.numeric(-round(GL.gsheet[["updated"]] - GL.gsheet[["reg_date"]],0))
  
  # print sheet details
  cat("\n")
  cat("   Title:\t",GL.gsheet.title,sep="")
  cat(" Updated:\t",GL.gsheet.updated_mins_ago," minutes ago.",sep="")
  cat("     URL:\t",GL.gsheet.url,sep="")
  cat("\n")
  
  # list worksheets
  #gs_ws_ls(GL.gsheet)
  
  # get TeamAssets worksheet
  GL.gsheet.TeamAssets <- gs_read(ss=GL.gsheet, ws = "TeamAssetsExtract")
  
  # review
  cat("\n  2018 Draft Picks Owned:\n---------------------------");Sys.sleep(0.2)
  print(GL.gsheet.TeamAssets);Sys.sleep(0.5)
  
  # convert to data.frame
  GL.gsheet.TeamAssets <- as.data.frame(GL.gsheet.TeamAssets)
  
  # remove NAs
  GL.gsheet.TeamAssets[is.na(GL.gsheet.TeamAssets)] <- 0
  
  # True Draft Picks Owned:
  current_draft_picks <- GL.gsheet.TeamAssets
  
  # Return
  current_draft_picks
  
}


#- Calculate value of a *single team*'s draft pick assets
Calc_ValueOfDraftPicks <- function(GL.data, draftpick.values=read.csv("draftpick_values.csv"), owner="Trevor", hurry=FALSE) {
  dps <- GL.data # rename for glob var clarity
  if(hurry==TRUE){runslow<-0}else{runslow<-1}
  # (add missing rounds)
  draftpick.values[c(165:168),] <- data.frame(c(165:168),14,c(9:12),1)
  draftpick.values[c(169:180),] <- data.frame(c(169:180),15,c(1:12),1)
  draftpick.values[c(181:192),] <- data.frame(c(181:192),16,c(1:12),1)
  # Get all draft picks
  dps <- dps %>% lapply(`[[`, "DraftPicks")
  dps <- dps %>% lapply(function(df) as.numeric(unlist(df)))
  dps <- as.data.frame(dps)
  
  
  # Limit to requested owner's team (***)
  dps_owner <- as.data.frame(dps)[[owner]]
  
  
  # Print
  cat(owner,"'s 2018 Draft Picks Owned:\n",sep="");Sys.sleep(runslow*1)
  for (d in 1:length(dps_owner)) {
    if (is.na(dps_owner[d])) {
      dps_owner[d] <- 0
      cat("\n")
    } else {
      cat("\t",if(dps_owner[d]>0) {dps_owner[d]})
      Sys.sleep(runslow*0.1+0.02)
    }
  }
  # Calculate value
  cumval <- 0
  for (dpv in 1:length(dps_owner)) {
    if(dps_owner[dpv]>0) {
      picknum <- dps_owner[dpv]
      pickval <- draftpick.values[picknum,"Value"]
      cumval <- cumval + pickval
    }
  }
  # Print
  cat(rep(" ",nchar(owner)+3),"Total Draft Clout:\n",sep="");Sys.sleep(1*runslow+0.1)
  for (cv in c(1:cumval)) {cat("\r\t",cv);Sys.sleep(runslow*0.002+0.0003)}
  Sys.sleep(1*runslow+0.1)
  cat("\n\n")
  # Return
  cumval
}


#- Print a clean copy of the summary table
PrintSummaryTable <- function(sTable,return=TRUE) {
  # Mush data
  test.table.clean <- test.table
  test.table.clean <- test.table.clean %>% select(Owner,TotADPVal,DraftCapital)
  test.table.clean <- mutate(test.table.clean, TotalTeamRating = TotADPVal + DraftCapital)
  names(test.table.clean) <- c("Team","Value_of_Keepers","Value_of_Draft_Picks","Overall_Team_Rating")
  test.table.clean <- test.table.clean %>% arrange(desc(Overall_Team_Rating))
  test.table.clean <- test.table.clean %>% select(Team,Overall_Team_Rating,Value_of_Draft_Picks,Value_of_Keepers)
  
  # Print
  cat("\nSummary table:\n");Sys.sleep(0.1)
  print(test.table.clean)
  
  # Plot
  par(mar = c(2.5, 4.5, 3, 2)) 
  barplot(test.table.clean$Overall_Team_Rating, col = c(rep("green",3),rep("yellow",3),rep("orange",3),rep("red",3)), width = 0.84, yaxp=c(0,max(test.table.clean$Overall_Team_Rating),400), horiz=TRUE, xlab="Overall Team Rating",ylab="Team",main="GL Team Strength (2018)\nPre-Draft",sub="Ratings powered by Sandermetrics", cex.axis=0.8, cex.lab=1, cex.main=1.1, cex.sub=1)
  axis(2, at=c(1:12), labels=test.table.clean$Team, col.axis="blue", las=2, tck=-0.02, cex.axis=0.8)
  
  # Return
  test.table.clean
}




#################################################################################




#################
##   SCORING   ##
##    RULES    ##
#################

ppr = 1.0

GL.ScoringRules <- list(
  QB = data.table::data.table(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                              multiplier = c(1/25, 4, -2, 1/10, 6, 2, -2 )),
  RB = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                              multiplier = c(1/10, 6, ppr, 1/8, 6, 6, 2, -2)),
  WR = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                              multiplier = c(1/10, 6, ppr, 1/8, 6, 6, 2, -2)),
  TE = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                              multiplier = c(1/10, 6, ppr, 1/8, 6, 6, 2, -2)),
  K = data.table::data.table(dataCol = c("xp", "fg0019", "fg2029", "fg3039", "fg4049", "fg50","fgMiss"),
                             multiplier = c(1, 3, 3, 3, 4, 5, -1)),
  DST = data.table::data.table(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                               multiplier = c(2, 2, 2, 1, 6, 2)),
  DL = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                              multiplier = c(0, 0, 0, 0, 0, 0, 0, 0, 0)),
  LB =  data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                               multiplier = c(0, 0, 0, 0, 0, 0, 0, 0, 0)),
  DB = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                              multiplier = c(0, 0, 0, 0, 0, 0, 0, 0, 0)),
  ptsBracket = data.table::data.table(threshold = c(0, 1, 7, 14, 18, 28, 35, 46, 99),
                                      points = c(5, 5, 4, 3, 1, 0, -1, -3, -5))
) 



#################
##    SCRAPE   ##
##     DATA    ##
#################

trace("mflPlayers",edit=TRUE) # in url: replace "football." with "www305."
scrapeData <- runScrape(week = 0, season = 2018, 
                        analysts = NULL,
                        positions = c("QB", "RB", "WR", "TE", "K", "DST")
)
##  Analysts
##      FAIL:  Yahoo, FOX, RTSports.com, 4for4, ...
##      GOOD:  FFToday, RTSports.com, ... (5 22)



#################
##  CALCULATE  ##
## PROJECTIONS ##
#################

trace("getRanks",edit=TRUE) # Line 82-83: Force the if-statement to TRUE
GL.projections <- getProjections(scrapeData, avgMethod = "average", leagueScoring = GL.ScoringRules, teams = 12, format = "ppr", 
                                 mflMocks = -1, mflLeagues = -1, 
                                 adpSources = c("CBS", "ESPN", "FFC", "MFL", "NFL")
)










