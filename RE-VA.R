library(dplyr)
library(retrosheet) 
library(rvest)
library(stringr)
library(rPython)
library("devtools")
library(stattleshipR)
setwd("~/Desktop/run-expectancy")
# update year at start of each season
year <- '2016' 
# these are all necessary csvs -- see readMe file for descriptions of each 
teams <- read.csv("teams2016.csv", header = TRUE)
dimnames(teams)[[1]] <- teams$X
# remove counting column
teams$X <- NULL
# python.load is to load python files
suppressWarnings(python.load("ALstandings.py"))
suppressWarnings(python.load("NLstandings.py"))
# read the csv that the two league standings produces 
AL.records <- read.csv(python.call("get_AL_standings", year), header = TRUE)
NL.records <- read.csv(python.call("get_NL_standings", year), header = TRUE)
# put together records file with both league standings
records <- rbind(AL.records, NL.records)
game.logs <- read.csv("gamelogs.merged.csv", header = FALSE)[,1:12]
headers <- read.csv("fields2.csv")
names(game.logs) <- headers$Header
splits <- read.csv("splits.csv", header = TRUE)
pitcherSplits <- read.csv("pitcherData-splits.csv", header = TRUE)
dimnames(pitcherSplits)[[1]] <- pitcherSplits$X
pitcherSplits$X <- NULL
pitcherData <- read.csv("pitcherData2016.csv", header=TRUE)
dimnames(pitcherData)[[1]] <- pitcherData$X
pitcherData$X <- NULL
# count.state is the file that is produced from run expectancy matrix with states and counts from 1990 - 2015
count.state <- read.csv("1990.2015.RE.States.Count.csv")
dimnames(count.state)[[1]] <- count.state[, 1]
wpstates <- read.csv("wpstates.csv", header = TRUE)
dimnames(wpstates)[[2]] <- c("State","WP",'Num')
# guts_table is wOBA stats from ESPN
guts_table <- read.csv("guts_table_2016.csv", header = TRUE)
dimnames(guts_table)[[1]] <- guts_table$X
guts_table$X <- NULL
Run.Env <- read.csv("Run.Environments.csv", header = TRUE)
player.info <- read.csv("player-info.csv", header=TRUE)
player.info$X <- NULL
deltaState <- read.csv("deltaState.csv", header=TRUE)
library("devtools")
devtools::install_github("stattleship/stattleship-r")
library(stattleshipR)
set_token("bf3c65fd3952ea434f4a96b641744475")

################################

# BPF function is to get batting park factors function using the past three years of data (as long as the stadium/park is at least three years old)
# should only need to be run at the start of each season
# see http://www.baseball-reference.com/about/parkadjust.shtml for math
get.BPF <- function(year, NT, home.team){
  # home team abbreviation
  home.team <- as.character(teams[(home.team), 1])
  # home game logs
  game.logs.home <- subset(game.logs, game.logs$HOME_TEAM == home.team & substr(game.logs$DATE,1,4) == as.character(year) & game.logs$VISITING_LEAGUE==game.logs$HOME_LEAGUE)
  # away game logs
  game.logs.away <- subset(game.logs, game.logs$VISITING_TEAM == home.team & substr(game.logs$DATE,1,4) == as.character(year) & game.logs$VISITING_LEAGUE==game.logs$HOME_LEAGUE)
  # get home league
  homeleague <- ifelse(game.logs.home[1,8] == 'AL', 1, 0)
  # sum number of runs scored at home by team X
  homeRS <- sum(game.logs.home$HOME_SCORE + game.logs.home$VISITING_SCORE)
  # sum number of games at home park
  home.games <- nrow(game.logs.home)
  # average runs scored at home park
  avg.home.RS <- homeRS/home.games
  # sum number of runs scored away by team X
  awayRS <-  sum(game.logs.away$HOME_SCORE + game.logs.away$VISITING_SCORE)
  # sum number of games away
  away.games <- nrow(game.logs.away)
  # average runs scored on the road (away)
  avg.away.RS <- awayRS/away.games
  # basic PF is avg runs scored at home divided by avg runs scored away 
  PF <- avg.home.RS/avg.away.RS
  # sum wins at home
  WAH <- sum(ifelse(game.logs.home$HOME_SCORE > game.logs.home$VISITING_SCORE,1,0))
  # sum loses on the road
  LOR <- sum(ifelse(game.logs.away$HOME_SCORE > game.logs.away$VISITING_SCORE,1,0))
  # innings pitched corrector
  IPC <- (18.5 - (WAH/home.games))/(18.5 - (LOR/away.games))
  Run.Factor <- PF/IPC
  # other parks corrector
  OPC <- NT/(NT-1 + Run.Factor)
  # scoring factor
  SF <- Run.Factor * OPC
  # scoring factor for other clubs
  SF1 <- 1 - abs((SF - 1)/NT)
  # runs per game scored at home
  RHT <- sum(game.logs.home$HOME_SCORE)/home.games
  # runs per game scored away
  RAT <- sum(game.logs.away$VISITING_SCORE)/away.games
  # runs per game allowed at home
  OHT <- sum(game.logs.home$VISITING_SCORE)/home.games
  # runs per game allowed away
  OAT <- sum(game.logs.away$HOME_SCORE)/away.games
  all.games <- subset(game.logs, substr(game.logs$DATE,1,4) == as.character(year) & game.logs$VISITING_LEAGUE==game.logs$HOME_LEAGUE)
  all.games$OVERALL_LEAGUE <- ifelse(all.games$HOME_LEAGUE=='AL', 1, 0)
  all.league.games <- subset(all.games, all.games$OVERALL_LEAGUE==homeleague)
  # Runs per game for all games in the league
  RAL <- sum(all.league.games$HOME_SCORE + all.league.games$VISITING_SCORE)/nrow(all.league.games)
  rawTPR <- 1
  TBR <- (abs(RAT/SF1 + RHT/SF)*abs(1 + (rawTPR - 1)/(NT - 1)))/RAL
  # team pitching rating
  TPR <- (abs(OAT/SF1 + OHT/SF)*abs(1 + (TBR - 1)/(NT - 1)))/RAL
  # final batting park factor
  BPF <- (SF + SF1)/(2 * abs(1 + (TPR - 1)/(NT - 1)))
  return(BPF)
}
# park.factors runs get.BPF ideally with past three years of data, but depending on when the park/stadium was built, it runs with as many years
# up to three years as possible
park.factors <- function(home.team, year, teams, game.logs){
  park <- teams[as.character(home.team), 6]
  year.built <- as.numeric(teams[as.character(home.team), 7])
  if(year.built <= (year - 2)){
    years <- list(year - 2, year - 1, year)
  }else{
    if(year.built <= (year - 1)){
      years <- list(year - 1, year)
    }else{years <- list(year)}
  }
  # number of teams in the league (update teams csv is number of teams change/team name changes/etc)
  NT <- sum(teams$league == teams[as.character(home.team),3])
  BPF <- mean(mapply(get.BPF, years, NT, home.team))
  return(BPF)
}

#calculates wOBA for player
player.wOBA <- function(batter.team, player_id, splits, table = guts_table, year = '2016'){
  # install github stattleship package and source 3 stattleship files from folder
  devtools::install_github("stattleship/stattleship-r")
  source("zzz.R")
  source("set_token.R")
  source("ss_get_result.R")
  # API given by stattleship site
  set_token("bf3c65fd3952ea434f4a96b641744475")
  # set parameters for stattleship search
  sport <- 'baseball'
  league <- 'mlb'
  ep <- 'game_logs'
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(player_id = player_id, status='ended', interval_type='regularseason')
  gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  ep <- 'players'
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE) 
  players<-do.call('rbind', lapply(pls, function(x) x$players))  
  colnames(players)[1] <- 'player_id' 
  game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs)) 
  # if not a valid player, return an empty data frame. Otherwise, calculate wOBA
  if(is.null(game_logs$player_id)){
    empty <- data.frame()
    return(empty)
  }else{
    game_logs <- merge(players, game_logs, by='player_id') 
    batter.stats <-  
      game_logs %>%
      filter(game_played == TRUE | batters_faced > 0) %>%
      group_by(name, bats, position_abbreviation) %>%
      summarise(PA = (sum(at_bats)+sum(walks)+sum(hit_by_pitch)+sum(sacrifice_flys)+sum(sacrifice_hits)), BB = sum(walks), HBP = sum(hit_by_pitch), singles = sum(singles), doubles = sum(doubles), triples = sum(triples), hr = sum(home_runs), SB = sum(stolen_bases), CS = sum(caught_stealing), AB = sum(at_bats), IBB = sum(intentional_walks_against), SF = sum(sacrifice_flys), SH = sum(sacrifice_hits)) 
    # bdata (batter data) with necessary stats to calculate wOBA
    bdata <- data.frame(batter.stats)
    # get current season stats from wOBA ESPN table (gut_table)
    season <- table[year, ]
    bdata <- merge(bdata, season)
    # regress data if less than 220 plate appearances (but more than one)
    bdata$regress <- ifelse(bdata$PA < 220 & bdata$PA > 0, TRUE, FALSE)
    # calculate raw wOBA from batters stats
    bdata$woba <- ifelse(bdata$PA != 0,round(((with(bdata, (wBB * (BB) + (wHBP * (HBP)) + (w1B * (singles)) + (w2B * (doubles)) + (w3B * (triples)) + (wHR * (hr)))/((AB) + (BB) - (IBB) + (HBP) + (SH) + (SF))))),4), (ifelse(bdata$position_abbreviation =='RP'|bdata$position_abbreviation =='SP', (average.pitcher.woba(splits, guts_table, (as.numeric(year)-1))), ifelse(bdata$position_abbreviation == 'C', (average.catcher.woba(splits, guts_table, (as.numeric(year)-1))),(average.woba(splits, guts_table, (as.numeric(year)-1)))))))
    # split regressed vs. non regressed 
    regress.data <- subset(bdata, bdata$regress == TRUE)
    bdata <- subset(bdata, bdata$regress != TRUE)
    # calculate woba/PA for batters that need regression
    regress.data$z <- as.numeric(regress.data$woba)/as.numeric(regress.data$PA)
    # split by position and regress to mean (of players with same position)
    pitchers <- new.data(subset(regress.data, regress.data$position_abbreviation == 'SP'|regress.data$position_abbreviation == 'RP'), 1, guts_table, year, splits)
    catchers <- new.data(subset(regress.data, regress.data$position_abbreviation == 'C'), 2, guts_table, year, splits)
    first.base <- new.data(subset(regress.data, regress.data$position_abbreviation == '1B'), 3, guts_table, year, splits)
    second.base <- new.data(subset(regress.data, regress.data$position_abbreviation == '2B'), 4, guts_table, year, splits)
    third.base <- new.data(subset(regress.data, regress.data$position_abbreviation == '3B'), 5, guts_table, year, splits)
    SS <- new.data(subset(regress.data, regress.data$position_abbreviation == 'SS'), 6, guts_table, year, splits)
    LF <- new.data(subset(regress.data, regress.data$position_abbreviation == 'LF'), 7, guts_table, year, splits)
    CF <- new.data(subset(regress.data, regress.data$position_abbreviation == 'CF'), 8, guts_table, year, splits)
    RF <- new.data(subset(regress.data, regress.data$position_abbreviation == 'RF'), 9, guts_table, year, splits)
    DH <- new.data(subset(regress.data, regress.data$position_abbreviation == 'DH'), 10, guts_table, year, splits)
    # combine rows from each set of data frames and return batter data
    bdata <- rbind(bdata, pitchers, catchers, first.base,second.base,third.base,SS,LF,CF,RF,DH)
    dimnames(bdata)[[1]] <- c(bdata$name)
    bdata$name <- NULL
    return(bdata)
  }
}
# get.coeffs is regression to mean function using position to get an equation
get.coeffs <- function(defensive.position, splits, table, year = '2016'){
  psplits <- subset(splits, splits$BAT_FLD_CD == defensive.position & splits$YEAR == as.numeric(year) - 1)
  # calculate number of singles
  psplits$B_1B <- with(psplits, B_H - B_2B - B_3B - B_HR)
  # woba stats from current year from guts table (ESPN)
  table <-table[as.character(as.numeric(year) - 1), ]
  # calculate wOBA
  psplits$woba <- round(((with(table, (wBB * (psplits$B_BB) + (wHBP * (psplits$B_HP)) + (w1B * (psplits$B_1B)) + (w2B * (psplits$B_2B)) + 	(w3B * (psplits$B_3B)) + (wHR * (psplits$B_HR)))/((psplits$B_AB) + (psplits$B_BB) - (psplits$B_IBB) + (psplits$B_HP) + (psplits$B_SH) + (psplits$B_SF))))),4)
  # calculate woba/PA
  psplits$z <- psplits$woba/psplits$B_PA
  # run the regression and get the coefficients
  coeff <- summary(lm(psplits$woba ~ psplits$z))$coefficients
  return(c(coeff[1,1], coeff[2,1]))
}
# new.data is to run the above function. Then using the coefficients, calculate the regressed woba
new.data <- function(position.data, defensive.position, table = guts_table, year = '2016', splits){
  position <- get.coeffs(defensive.position, splits, guts_table, '2016')
  position.data$woba <- position[1] + position[2]*as.numeric(position.data$woba)/position.data$PA
  position.data$z <- NULL
  position.data$retrosheet.id <- NULL
  return(position.data)
}
# average.woba is function to calculate the average wOBA in the league for players who have zero plate appearances
average.woba <- function(splits, table = guts_table, year = '2015'){
  osplits <- subset(splits, splits$BAT_FLD_CD != 1 & splits$BAT_FLD_CD != 2)
  # calculate number of singles 
  osplits$B_1B <- with(osplits, B_H - B_2B - B_3B - B_HR)
  # get current year of stats from guts_table (current woba stats from ESPN)
  table <-table[as.character(as.numeric(year) - 1), ]
  # calculate wOBA
  osplits$woba <- round(((with(table, (wBB * (osplits$B_BB) + (wHBP * (osplits$B_HP)) + (w1B * (osplits$B_1B)) + (w2B * (osplits$B_2B)) + 	(w3B * (osplits$B_3B)) + (wHR * (osplits$B_HR)))/((osplits$B_AB) + (osplits$B_BB) - (osplits$B_IBB) + (osplits$B_HP) + (osplits$B_SH) + (osplits$B_SF))))),4)
  # if infinite is returned by above calculation, set to zero
  osplits[osplits$woba == "Inf",]$woba = 0
  return(mean(osplits$woba, na.rm = TRUE))
}
# average.pitcher.woba is function to calculate the average pitcher wOBA in the league for pitchers who have zero plate appearances 
# (same steps as above function)
average.pitcher.woba <- function(splits, table = guts_table, year = '2015'){
  psplits <- subset(splits, splits$BAT_FLD_CD == 1)
  psplits$B_1B <- with(psplits, B_H - B_2B - B_3B - B_HR)
  table <- table[2, ]
  avg.pitcher.woba <- round(((with(table, (wBB * sum(psplits$B_BB) + (wHBP * sum(psplits$B_HP)) + (w1B * sum(psplits$B_1B)) + (w2B * sum(psplits$B_2B)) + 	(w3B * sum(psplits$B_3B)) + (wHR * sum(psplits$B_HR)))/(sum(psplits$B_AB) + sum(psplits$B_BB) - sum(psplits$B_IBB) + sum(psplits$B_HP) + sum(psplits$B_SH) + sum(psplits$B_SF))))),4)
  return(avg.pitcher.woba)
}
# average.catcher.woba is function to calculate the average catcher wOBA in the league for catchers who have zero plate appearances 
# (same steps as above function)
average.catcher.woba <- function(splits, table = guts_table, year = '2015'){
  csplits <- subset(splits, splits$BAT_FLD_CD == 2)
  csplits$B_1B <- with(csplits, B_H - B_2B - B_3B - B_HR)
  table <- table[2, ]
  avg.catcher.woba <- round(((with(table, (wBB * sum(csplits$B_BB) + (wHBP * sum(csplits$B_HP)) + (w1B * sum(csplits$B_1B)) + (w2B * sum(csplits$B_2B)) + 	(w3B * sum(csplits$B_3B)) + (wHR * sum(csplits$B_HR)))/(sum(csplits$B_AB) + sum(csplits$B_BB) - sum(csplits$B_IBB) + sum(csplits$B_HP) + sum(csplits$B_SH) + sum(csplits$B_SF))))),4)
  return(avg.catcher.woba)
}
# get retrosheet id for a given pitcher (with current team) to get pitcher stats
get.retrosheet.id <- function(pitcher.name, pitcher.team, pitcherData){
  info <- paste(pitcher.team, pitcher.name)
  # find retrosheet id using pitcher's name and current team
  retrosheet.id <- pitcherData[info, 3]
  return(retrosheet.id)
}
# get run expectancy with various variables as inputs
get.run.expectancy <- function(home.team, batter.team, batter.name, pitcher.name, pitcher.team, year = '2016', psplits = pitcherSplits, table = guts_table, team.info = teams){
  # get park factors from previous three years (can't use current year's data)
  pfyear <- as.numeric(year) - 1
  BPF <- park.factors(home.team, pfyear, team.info, game.logs)
  # get home team, batter team, and pitcher team abbreviations
  home.team <- as.character(team.info[home.team, 1])
  batter.team <- team.info[batter.team, 2]
  pitcher.team <- team.info[pitcher.team, 5]
  #pitching stats -- get L/R splits (wOBA)
  pitcher.retrosheet.id <- as.character(get.retrosheet.id(pitcher.name, pitcher.team, pitcherData))
  p.splits <- psplits[, pitcher.retrosheet.id]
  p.splits.R <- ifelse(p.splits[1] == 0, p.splits[3], p.splits[1])
  p.splits.L <- ifelse(p.splits[2] == 0, p.splits[3], p.splits[2])
  avg <- p.splits[3]
  # for players with names like "C.J.", need to remove periods to run with stattleship
  name <-  gsub("\\."," ", batter.name)
  name <- gsub("  ", " ", name)
  team <- as.character(team.info[batter.team, 8])
  # if batter's name in in the player.info csv, use the given slug name. Otherwise, use the generic "mlb-firstname-lastname"
  player_id <- ifelse(batter.name %in% player.info$name, as.character(subset(player.info, team == player.info$team & player.info$name == batter.name)$slug), tolower(paste('mlb', as.character(gsub(" ", "-", name)), sep ="-")))
  # get stat line (and calculate wOBA) for given player
  stats <- player.wOBA(batter.team, player_id, splits, table, '2016')
  # get current league wOBA and the scale for run value per plate appearance calculation
  league.wOBA <- table[year,1]
  current.scale <- table[year,2]
  # if an empty stat line is returned, use all the averages to calculate run value per plate appearance
  if(nrow(stats) == 0){
    opp.woba <- avg
    avg.woba <- average.woba(splits, table = guts_table, year = '2015')
    RV.Per.PA <- round(((as.numeric(avg.woba) - as.numeric(league.wOBA))/current.scale) * BPF,4)
    return(RV.Per.PA)
  }else{
    # Otherwise use player's stats to calculate their run value per plate appearance and return necessary stats
    stats$opp.wOBA <- ifelse(stats$bats == 'bats_right', p.splits.R, (ifelse(stats$bats =='bats_left', p.splits.L, avg)))
    stats$avg.woba <- with(stats,(woba) - (woba * (woba - opp.wOBA)))
    stats$RV.Per.PA <- round(with(stats,(as.numeric(avg.woba) - as.numeric(lg_woba))/woba_scale) * BPF,4)
    stats <- stats[-c(4:29)]
    return(stats)
  }
}
# main function gets the run expectancy given specific factors
main.function <- function(state, count, batter, stats, env = Run.Env, condition = count.state){
  # calculate the run environment difference to correct the count.state csv values
  avg.run.env <- (sum(env[,2])/(nrow(env)))
  yr.run.env <- env[as.character(year),2]
  dif <- ((yr.run.env-avg.run.env)/avg.run.env)
  cs <- condition[state,count]
  RE.with.Run.Env <- ((cs)*(dif) + (cs))
  # if stats is of type "double", the average values were used, so entire stat line is not returned
  if(typeof(stats) == "double"){
    RE <- round(stats + RE.with.Run.Env, 2)
  }else{
    # otherwise get the RV/PA stat and add with run environment
    RE <- round(as.numeric(stats[, 7]) + RE.with.Run.Env, 2)
  }
  return(RE)
}
# fun1 gets run expectancy using the above function
fun1 <- function(home.team,batter.team,pitcher.team,pitcher.name,state,count,batter.name){
  # don't allow for not valid matchups
  if(batter.team == pitcher.team){
    return('Not a Valid Matchup')
  }else{
    stats <- get.run.expectancy(home.team,batter.team,batter.name, pitcher.name,pitcher.team)
    RE <- main.function(state,count,batter.name, stats)
    return(RE)
  }
}















# get percents of singles, doubles, triples, and homeruns out of all player's hits
get.percents <- function(batter.team, batter.name, team.info = teams){
  suppressWarnings(python.load("batterPercentages.py"))
  team <- as.character(team.info[batter.team,8])
  percents <- python.call("get_percentages", batter.name, team)
  return(percents)
} 
# get batting average for specific player
get.ba <- function(batter.team, batter.name, team.info){
  suppressWarnings(python.load("batterBA.py"))
  team <- as.character(team.info[batter.team,8])
  BA <- suppressWarnings(python.call("get_ba", batter.name, team))
  return(BA)
} 
# get league batting average
league.ba <- function(league){
  suppressWarnings(python.load("leagueBA.py"))
  if(league == 'inter'){
    al <- as.numeric(python.call("get_league_ba", 'AL'))
    nl <- as.numeric(python.call("get_league_ba", 'NL'))
    lg <- (al + nl)/2
  }else{    
    lg <- as.numeric(python.call("get_league_ba", league))
  }
  return(lg)
}
# get pitcher stat using stattleship
pitcher.stats <- function(p.stat, pitcher){
  league <- "mlb"
  sport <- "baseball"
  ep <- "stats"
  # p.stat is a specific stat
  q_body <- list(stat = p.stat, type = 'baseball_pitcher_stat', player_id = pitcher)
  player <- ss_get_result(sport = sport, league = league, ep = ep,
                          query = q_body, version = 1, walk = FALSE)
  players_df <- data.frame(player[[1]]$stats)
  x <- sum(players_df$stat, na.rm = TRUE)
  return(x)
}
# use above function to get all necessary stats to calculate batting average against for pitcher
get.baa <- function(pitcher.player.id, league){
  neededStats <- list('pitcher_hit_by_pitcher','pitcher_hits','pitcher_intentional_walks','pitchers_sacrifice_flys','pitcher_sacrifice_hits','pitcher_walks','batters_faced','catcher_interferences')
  stats <- mapply(pitcher.stats, neededStats, pitcher.player.id)
  # if pitcher has not faced any batters in the current season, use league averages for batting average against
  if(stats[7] == 0){
    suppressWarnings(python.load("get_league_baa.py"))
    baa <- as.numeric(python.call("get_league_baa", league))
    return(baa)
  }else{
    baa <- stats[2]/(stats[7] - (stats[6] + stats[3]) - stats[1] - stats[5] - stats[4] - stats[8])
    return(baa)
  }
}
# calculates specific win probability using pitcher/batter matchup
matchup <- function(batter.team, batter.name, pitcher.player.id,wpstates, half.inning, state, count, rdiff, home.team, visiting.team, pitcher.name, pitcher.team, cs, records, team.info, league){
  # get batter's batting average
  x <- as.numeric(get.ba(batter.team, batter.name, team.info))
  # get pitcher's batting average against
  y <- as.numeric(get.baa(pitcher.player.id, league))
  # get league batting average
  z <- league.ba(league)
  # use log5 formula to calculate probability of a hit for the batter
  exAVG <- ((x*y)/z)/(((x*y)/z) + ((1 -x)*(1-y))/(1-z))
  # 
  deltaAVG <- (exAVG - z) + 1
  # get percentages of singles, double, triples, homeruns from number of hits
  batterPercentages <- as.numeric(get.percents(batter.team, batter.name))
  # probability of hitting a single 
  psingle <- batterPercentages[1] * deltaAVG
  # probability of hitting a double 
  pdouble <- batterPercentages[2] * deltaAVG
  # probability of hitting a triple 
  ptriple <- batterPercentages[3] * deltaAVG
  # probability of hitting a homerun 
  phr <- batterPercentages[4] * deltaAVG
  # probability of getting an out
  pout <- 1 - (psingle + pdouble + ptriple + phr)
  # if current number of outs = 2, if an out is made, state goes to "000 0" and a new half inning. 
  if(substr(state,5,5)=='2'){
    out.state <- str_replace(state, '2', '0')
    out.state <- str_replace(state, substr(state,1,3), '000')
    # find the next half inning
    if(substr(half.inning,5,5)=='1'){
      out.half.inning <- paste(as.character(as.numeric(substr(half.inning,1,1))+1), '0')
      out.half.inning <- min(9, out.half.inning)
    }else if(substr(half.inning,5,5)=='0'){
      out.half.inning <- paste(substr(half.inning,1,1), '1')
    }
  }else{
    # otherwise, calculate the new out.state and the half inning would stay the same
    # x is the current number of outs and y is if an out is made
    x <- as.numeric(substr(state, 5, 5))
    y <- x + 1
    out.state <- paste(substr(state,1,4),  as.character(y), sep = "")
    out.half.inning <- half.inning
  } 
  # new state if a single is hit
  s.state <- as.character(subset(deltaState, deltaState$State == state)$Single)
  # new state if a double is hit
  d.state <- as.character(subset(deltaState, deltaState$State == state)$Double)
  # new state if a triple is hit
  t.state <- as.character(subset(deltaState, deltaState$State == state)$Triple)
  # new state if a home run is hit
  hr.state <- as.character(subset(deltaState, deltaState$State == state)$HR)
  # run differential if a single is hit
  s.rdiff <- get.s.rdiff(state, half.inning, rdiff)
  # run differential if a double is hit
  d.rdiff <- get.d.rdiff(state, half.inning, rdiff)
  # run differential if a triple is hit
  t.rdiff <- get.t.rdiff(state, half.inning, rdiff)
  # run differential if a home run is hit
  hr.rdiff <-  get.hr.rdiff(state, half.inning, rdiff)
  # calculate the current Win Probability using WP function
  # currentWP <- WP(wpstates, half.inning, state, count, rdiff, home.team, visiting.team, cs, records, team.info)
  currentWP <- corrector(half.inning, state, rdiff, wpstates)
  # calculate the win probability if a single is hit
  # WPsingle <- WP(wpstates, half.inning, s.state, 'c00', s.rdiff, home.team, visiting.team, cs, records, team.info)
  WPsingle <- corrector(half.inning, s.state, s.rdiff, wpstates)
  WPsingle[is.na(WPsingle)] <- 0
  # calculate the win probability if a double is hit
  # WPdouble <- WP(wpstates, half.inning, d.state, 'c00', d.rdiff, home.team, visiting.team, cs, records, team.info)
  WPdouble <- corrector(half.inning, d.state, d.rdiff, wpstates)
  WPdouble[is.na(WPdouble)] <- 0
  # calculate the win probability if a triple is hit
  # WPtriple <- WP(wpstates, half.inning, t.state, 'c00', t.rdiff, home.team, visiting.team, cs, records, team.info)
  WPtriple <- corrector(half.inning, t.state, t.rdiff, wpstates)
  WPtriple[is.na(WPtriple)] <- 0
  # calculate the win probability if a home run is hit
  # WPhr <- WP(wpstates, half.inning, hr.state, 'c00', hr.rdiff, home.team, visiting.team, cs, records, team.info)
  WPhr <- corrector(half.inning, hr.state, hr.rdiff, wpstates)
  WPtriple[is.na(WPtriple)] <- 0
  # calculate the win probability if an out is made
  # WPout <- WP(wpstates, out.half.inning, out.state, 'c00', rdiff, home.team, visiting.team, cs, records, team.info)
  WPout <- corrector(half.inning, out.state, rdiff, wpstates)
  # sum the differences in win probabilities for each scenario multiplied by their corresponding probabilities
  if(half.inning == '9 0' & substr(state, 5,5) == 2 & rdiff > 0){
    WPout <- 0
  }
  if(half.inning == '9 1'){
    WPsingle <- ifelse(s.rdiff > 0, 1, WPsingle)
    WPdouble <- ifelse(d.rdiff > 0, 1, WPdouble)
    WPtriple <- ifelse(t.rdiff > 0, 1, WPtriple)
    WPhr <- ifelse(hr.rdiff > 0, 1, WPhr)
  }
  delta <- ((WPout - currentWP)*pout) + ((WPsingle - currentWP)*psingle) + ((WPdouble - currentWP)*pdouble) + ((WPtriple - currentWP)*ptriple) + ((WPhr - currentWP)*phr)
  # return the new win probability given the pitcher/batter calculations
  newWP <- (as.numeric(currentWP*100)) + delta
  return(newWP/100)
}
# calculate run differential if a single is hit using current state
get.s.rdiff <- function(state, half.inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  return(rdiff)
}
# calculate run differential if a double is hit using current state
get.d.rdiff <- function(state, half.inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,2,2) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  return(rdiff)
}
# calculate run differential if a triple is hit using current state
get.t.rdiff <- function(state, half.inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,2,2) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,1,1) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  return(rdiff)
}
# calculate run differential if a homerun is hit using current state
get.hr.rdiff <- function(state, half.inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,2,2) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,1,1) == 1, ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1)
  return(rdiff)
}
# run matchup function using all variables
pitcher.batter.WP <- function(wpstates, half.inning, state, count, rdiff, home.team, visiting.team, pitcher.name, batter.name, pitcher.team, batter.team, cs, records, team.info, league){
  if(rdiff > 0 & (half.inning == '1 0'|half.inning == '9 1')){
    return('Impossible')
  }else{
    # correct for nicknames like "C.J." now for pitchers
    name <-  gsub("\\."," ", pitcher.name)
    name <- gsub("  ", " ", name)
    # get team abbreviation 
    team <- as.character(team.info[pitcher.team, 8])
    # if pitcher is in list of current players, get corresponding slug name, otherwise use generic form of "mlb-firstname-lastname"
    pitcher.player.id <- ifelse(pitcher.name %in% player.info$name, as.character(subset(player.info, team == player.info$team & player.info$name == pitcher.name)$slug), tolower(paste('mlb', as.character(gsub(" ", "-", name)), sep ="-")))
    # run matchup function and return specific win probability given pitcher and batter
    pitcher.batter.matchup <- matchup(batter.team, batter.name, pitcher.player.id, wpstates, half.inning, state, count, rdiff, home.team, visiting.team, pitcher.name, pitcher.team, cs, records, team.info, league)
    win.prob <- standings(home.team, visiting.team, teams, records, pitcher.batter.matchup, half.inning)
    return(win.prob) 
  }
}

# change NA results from calling WP to zero
 
state.correction <- function(half.inning, state, rdiff, wpstates){
  states <- c('000 2','100 2','000 1','010 2','001 2','110 2','101 2','000 0','100 1','011 2','010 1','111 2','100 0','110 1','001 1','010 0','101 1','011 1','001 0','110 0','111 1','101 0','011 0','111 0')
  perm <- paste(half.inning, state, rdiff, sep = " ")
  perms <- c(paste(half.inning,states[1],rdiff,sep = " "),paste(half.inning,states[2],rdiff,sep = " "),paste(half.inning,states[3],rdiff,sep = " "),paste(half.inning,states[4],rdiff,sep = " "),paste(half.inning,states[5],rdiff,sep = " "),paste(half.inning,states[6],rdiff,sep = " "),paste(half.inning,states[7],rdiff,sep = " "),paste(half.inning,states[8],rdiff,sep = " "),paste(half.inning,states[9],rdiff,sep = " "),paste(half.inning,states[10],rdiff,sep = " "),
             paste(half.inning,states[11],rdiff,sep = " "),paste(half.inning,states[12],rdiff,sep = " "),paste(half.inning,states[13],rdiff,sep = " "),paste(half.inning,states[14],rdiff,sep = " "),paste(half.inning,states[15],rdiff,sep = " "),paste(half.inning,states[16],rdiff,sep = " "),paste(half.inning,states[17],rdiff,sep = " "),paste(half.inning,states[18],rdiff,sep = " "),paste(half.inning,states[19],rdiff,sep = " "),paste(half.inning,states[20],rdiff,sep = " "),
             paste(half.inning,states[21],rdiff,sep = " "),paste(half.inning,states[22],rdiff,sep = " "),paste(half.inning,states[23],rdiff,sep = " "),paste(half.inning,states[24],rdiff,sep = " "))
  
  allstates <- c((subset(wpstates, wpstates$State == perms[1]))$WP,(subset(wpstates, wpstates$State == perms[2]))$WP,(subset(wpstates, wpstates$State == perms[3]))$WP,(subset(wpstates, wpstates$State == perms[4]))$WP,(subset(wpstates, wpstates$State == perms[5]))$WP,(subset(wpstates, wpstates$State == perms[6]))$WP,(subset(wpstates, wpstates$State == perms[7]))$WP,(subset(wpstates, wpstates$State == perms[8]))$WP,(subset(wpstates, wpstates$State == perms[9]))$WP,(subset(wpstates, wpstates$State == perms[10]))$WP,(subset(wpstates, wpstates$State == perms[11]))$WP,(subset(wpstates, wpstates$State == perms[12]))$WP,(subset(wpstates, wpstates$State == perms[13]))$WP,(subset(wpstates, wpstates$State == perms[14]))$WP,(subset(wpstates, wpstates$State == perms[15]))$WP,(subset(wpstates, wpstates$State == perms[16]))$WP,(subset(wpstates, wpstates$State == perms[17]))$WP,(subset(wpstates, wpstates$State == perms[18]))$WP,(subset(wpstates, wpstates$State == perms[19]))$WP,(subset(wpstates, wpstates$State == perms[20]))$WP,(subset(wpstates, wpstates$State == perms[21]))$WP,(subset(wpstates, wpstates$State == perms[22]))$WP,(subset(wpstates, wpstates$State == perms[23]))$WP,(subset(wpstates, wpstates$State == perms[24]))$WP)
  if(substr(half.inning,3,3) == '0'){
    sorted <- sort(allstates, decreasing = TRUE)
    index <- (as.numeric(which(perms == perm)))
    wpstate <- sorted[index]
  }else if(substr(half.inning,3,3) == '1'){
    sorted <- sort(allstates)
    index <- as.numeric(which(perms == perm))
    wpstate <- sorted[index]
  }
  ifelse(wpstate >= 1, return(0.9999), return(wpstate))
  ifelse(wpstate <= 0, return(0.0001), return(wpstate))
}

run.correction <- function(half.inning, state, rdiff, wpstates){
  if(substr(half.inning,3,3) == '0'){
    if(rdiff > 0){
      upper <- c(state.correction('2 0', state, rdiff, wpstates), state.correction('3 0', state, rdiff, wpstates),
                 state.correction('4 0', state, rdiff, wpstates), state.correction('5 0', state, rdiff, wpstates), state.correction('6 0', state, rdiff, wpstates),
                 state.correction('7 0', state, rdiff, wpstates), state.correction('8 0', state, rdiff, wpstates), state.correction('9 0', state, rdiff, wpstates))
      index <- as.numeric(substr(half.inning,1,1)) - 1 
      sorted <- sort(upper)
      updated <- sorted[index]
      return(min(updated, 0.9999))
    }else{
      upper <- c(state.correction('1 0', state, rdiff, wpstates), state.correction('2 0', state, rdiff, wpstates), state.correction('3 0', state, rdiff, wpstates),
                 state.correction('4 0', state, rdiff, wpstates), state.correction('5 0', state, rdiff, wpstates), state.correction('6 0', state, rdiff, wpstates),
                 state.correction('7 0', state, rdiff, wpstates), state.correction('8 0', state, rdiff, wpstates), state.correction('9 0', state, rdiff, wpstates))
      index <- as.numeric(substr(half.inning,1,1))
      sorted <- sort(upper, decreasing = TRUE)
      updated <- sorted[index]
      return(max(updated, 0.0001))
    }
  }else if(substr(half.inning,3,3) == '1'){
    if(rdiff > 0){
      lower <- c(state.correction('1 1', state, rdiff, wpstates), state.correction('2 1', state, rdiff, wpstates), state.correction('3 1', state, rdiff, wpstates),
                 state.correction('4 1', state, rdiff, wpstates), state.correction('5 1', state, rdiff, wpstates), state.correction('6 1', state, rdiff, wpstates),
                 state.correction('7 1', state, rdiff, wpstates), state.correction('8 1', state, rdiff, wpstates))
      index <- as.numeric(substr(half.inning,1,1)) 
      sorted <- sort(lower)
      updated <- sorted[index]
      return(min(updated, 0.9999))
    }else{
      lower <- c(state.correction('1 1', state, rdiff, wpstates), state.correction('2 1', state, rdiff, wpstates), state.correction('3 1', state, rdiff, wpstates),
                 state.correction('4 1', state, rdiff, wpstates), state.correction('5 1', state, rdiff, wpstates), state.correction('6 1', state, rdiff, wpstates),
                 state.correction('7 1', state, rdiff, wpstates), state.correction('8 1', state, rdiff, wpstates), state.correction('9 1', state, rdiff, wpstates))
      index <- as.numeric(substr(half.inning,1,1))
      sorted <- sort(lower, decreasing = TRUE)
      updated <- sorted[index]
      return(max(updated, 0.0001))
    }
  }
}

corrector <- function(half.inning, state, rdiff, wpstates){
  A <- state.correction(half.inning, state, rdiff, wpstates)
  B <- run.correction(half.inning, state, rdiff, wpstates)
  corrector <- (A+B)/2
  return(corrector)
}

standings <- function(home.team, visiting.team, teams, records, wpstate, half.inning){
  hleague <- teams[home.team, 3]
  # get league of away team
  aleague <- teams[visiting.team, 3]
  # get abbreviation of home team
  home.team <- teams[home.team,8]
  # get abbreviation of away team
  visiting.team <- teams[visiting.team,8]
  # if an interleague game, use a different set of calculations
  if(hleague != aleague){
    # get records for both teams (interleague record)
    hrecord <- subset(records, records$Team == home.team & records$Opponent == as.character(aleague))
    arecord <- subset(records, records$Team == visiting.team & records$Opponent == as.character(hleague))
    # if home team or away team hasn't played any games against a team from the other league, use their total record to calculate winning percentage
    if(hrecord$Wins + hrecord$Losses == 0){
      hrecord <- subset(records, records$Team == home.team)
      hpercentage <- sum(hrecord$Wins)/(sum(hrecord$Losses)+sum(hrecord$Wins))
    }
    if(arecord$Wins + arecord$Losses == 0){
      arecord <- subset(records, records$Team == home.team)
      apercentage <- sum(arecord$Wins)/(sum(arecords$Losses)+sum(arecords$Wins))
    }else{
      # otherwise just calculate winning percentage from interleague games
      hpercentage <- as.numeric(hrecord$Wins)/(as.numeric(hrecord$Losses)+as.numeric(hrecord$Wins))
      apercentage <- as.numeric(arecord$Wins)/(as.numeric(arecord$Losses)+as.numeric(arecord$Wins))
    }
    # calculate win probability for home team against the visiting team
    P.ab <- (hpercentage-apercentage)+0.5
  }else if(home.team == visiting.team){
    # if home screen is set (for example Angels v. Angels), return generic win probability
    P.ab <- 0.5
  }else{
    # if game is between teams in the same league, get record of both teams, and calculate total number of games each team has played
    record.a <- subset(records, records$Team == as.character(home.team))
    games.a <- as.numeric(sum(record.a$Wins) + sum(record.a$Losses))
    record.b <- subset(records, records$Team == as.character(visiting.team))
    games.b <- as.numeric(sum(record.b$Wins) + sum(record.b$Losses))
    # if no games have been played by one team, return generic win probability
    if(games.a == 0){
      P.ab <- 0.5
    }else if(games.b == 0){
      P.ab <- 0.5
      # otherwise find win probabilities for each team and use log5 equation for probability between two teams
    }else{
      P.a <- as.numeric(sum(record.a$Wins)/games.a)
      P.b <-as.numeric(sum(record.b$Wins)/games.b)
      P.ab <- (P.a - (P.a * P.b))/(P.a + P.b - (2*(P.a*P.b)))
    }
  }
  # find the amount above or below the basic 50/50 chances
  x <- P.ab - 0.50
  # value the team standings propotionally less as the game continues, and multiply by updated state to get the win probability with the team standings
  wp.with.team.standing <- as.numeric(wpstate)*(1+x/as.numeric(substr(half.inning,1,1)))
  if(wpstate > 0.90){
    wp.with.team.standing <- min(wp.with.team.standing, 0.9999)
  }else if(wpstate < 0.1){
    wp.with.team.standing <- max(wp.with.team.standing, 0.01)
  }
  return(round(wp.with.team.standing,3))
}









