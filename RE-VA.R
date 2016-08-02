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
AL_records <- read.csv(python.call("get_AL_standings", year), header = TRUE)
NL_records <- read.csv(python.call("get_NL_standings", year), header = TRUE)
# put together records file with both league standings
records <- rbind(AL_records, NL_records)
game_logs <- read.csv("gamelogs.merged.csv", header = FALSE)[,1:12]
headers <- read.csv("fields2.csv")
names(game_logs) <- headers$Header
splits <- read.csv("splits.csv", header = TRUE)
# count.state is the file that is produced from run expectancy matrix with states and counts from 1990 - 2015
count_state <- read.csv("1990.2015.RE.States.Count.csv")
dimnames(count_state)[[1]] <- count_state[, 1]
wpstates <- read.csv("wpstates.csv", header = TRUE)
dimnames(wpstates)[[2]] <- c("State","WP",'Num')
# guts_table is wOBA stats from ESPN
guts_table <- read.csv("guts_table_2016.csv", header = TRUE)
dimnames(guts_table)[[1]] <- guts_table$X
guts_table$X <- NULL
# Run environment is table of run environments from 1990-2015
Run_Env <- read.csv("Run.Environments.csv", header = TRUE)
# deltaState gives the new state if a single, double, etc. is hit
deltaState <- read.csv("deltaState.csv", header=TRUE)
activePlayers <- read.csv("activePlayers.csv", header=TRUE)
activePlayers$X <- NULL
pitcher_info <- read.csv("pitcher.info.csv", header=TRUE)
pitcher_info$X <- NULL
library("devtools")
devtools::install_github("stattleship/stattleship-r")
library(stattleshipR)
set_token("bf3c65fd3952ea434f4a96b641744475")

### Run Expectancy ###

#calculates wOBA for player
player_wOBA <- function(batter_team, player_id, splits, table = guts_table, year = '2016'){
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
  league_team <- paste(league, batter_team, sep = "-")
  q_body <- list(player_id = player_id, status='ended', interval_type='regularseason')
  gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  ep <- 'players'
  q_body <- list(team_id = league_team)
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
    batter_stats <-  
      game_logs %>%
      filter(game_played == TRUE | batters_faced > 0) %>%
      group_by(name, bats, position_abbreviation) %>%
      summarise(PA = (sum(at_bats)+sum(walks)+sum(hit_by_pitch)+sum(sacrifice_flys)+sum(sacrifice_hits)), BB = sum(walks), HBP = sum(hit_by_pitch), singles = sum(singles), doubles = sum(doubles), triples = sum(triples), hr = sum(home_runs), SB = sum(stolen_bases), CS = sum(caught_stealing), AB = sum(at_bats), IBB = sum(intentional_walks_against), SF = sum(sacrifice_flys), SH = sum(sacrifice_hits)) 
    # bdata (batter data) with necessary stats to calculate wOBA
    bdata <- data.frame(batter_stats)
    # get current season stats from wOBA ESPN table (gut_table)
    season <- table[year, ]
    bdata <- merge(bdata, season)
    # regress data if less than 220 plate appearances (but more than one)
    bdata$regress <- ifelse(bdata$PA < 220 & bdata$PA > 0, TRUE, FALSE)
    # calculate raw wOBA from batters stats
    bdata$woba <- ifelse(bdata$PA != 0,round(((with(bdata, (wBB * (BB) + (wHBP * (HBP)) + (w1B * (singles)) + (w2B * (doubles)) + (w3B * (triples)) + (wHR * (hr)))/((AB) + (BB) - (IBB) + (HBP) + (SH) + (SF))))),4), (ifelse(bdata$position_abbreviation =='RP'|bdata$position_abbreviation =='SP', (average_pitcher_woba(splits, guts_table, (as.numeric(year)-1))), ifelse(bdata$position_abbreviation == 'C', (average_catcher_woba(splits, guts_table, (as.numeric(year)-1))),(average_woba(splits, guts_table, (as.numeric(year)-1)))))))
    # split regressed vs. non regressed 
    regress_data <- subset(bdata, bdata$regress == TRUE)
    bdata <- subset(bdata, bdata$regress != TRUE)
    # calculate woba/PA for batters that need regression
    regress_data$z <- as.numeric(regress_data$woba)/as.numeric(regress_data$PA)
    # split by position and regress to mean (of players with same position)
    pitchers <- new_data(subset(regress_data, regress_data$position_abbreviation == 'SP'|regress_data$position_abbreviation == 'RP'), 1, guts_table, year, splits)
    catchers <- new_data(subset(regress_data, regress_data$position_abbreviation == 'C'), 2, guts_table, year, splits)
    first_base <- new_data(subset(regress_data, regress_data$position_abbreviation == '1B'), 3, guts_table, year, splits)
    second_base <- new_data(subset(regress_data, regress_data$position_abbreviation == '2B'), 4, guts_table, year, splits)
    third_base <- new_data(subset(regress_data, regress_data$position_abbreviation == '3B'), 5, guts_table, year, splits)
    SS <- new_data(subset(regress_data, regress_data$position_abbreviation == 'SS'), 6, guts_table, year, splits)
    LF <- new_data(subset(regress_data, regress_data$position_abbreviation == 'LF'), 7, guts_table, year, splits)
    CF <- new_data(subset(regress_data, regress_data$position_abbreviation == 'CF'), 8, guts_table, year, splits)
    RF <- new_data(subset(regress_data, regress_data$position_abbreviation == 'RF'), 9, guts_table, year, splits)
    DH <- new_data(subset(regress_data, regress_data$position_abbreviation == 'DH'), 10, guts_table, year, splits)
    # combine rows from each set of data frames and return batter data
    bdata <- rbind(bdata, pitchers, catchers, first_base,second_base,third_base,SS,LF,CF,RF,DH)
    dimnames(bdata)[[1]] <- c(bdata$name)
    bdata$name <- NULL
    return(bdata)
  }
}
# get_coeffs is regression to mean function using position to get an equation
get_coeffs <- function(defensive_position, splits, table, year = '2016'){
  psplits <- subset(splits, splits$BAT_FLD_CD == defensive_position & splits$YEAR == as.numeric(year) - 1)
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
# new_data is to run the above function. Then using the coefficients, calculate the regressed woba
new_data <- function(position_data, defensive_position, table = guts_table, year = '2016', splits){
  position <- get_coeffs(defensive_position, splits, guts_table, '2016')
  position_data$woba <- position[1] + position[2]*as.numeric(position_data$woba)/position_data$PA
  position_data$z <- NULL
  position_data$retrosheet_id <- NULL
  return(position_data)
}
# average_woba is function to calculate the average wOBA in the league for players who have zero plate appearances
average_woba <- function(splits, table = guts_table, year = '2015'){
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
# average_pitcher_woba is function to calculate the average pitcher wOBA in the league for pitchers who have zero plate appearances 
# (same steps as above function)
average_pitcher_woba <- function(splits, table = guts_table, year = '2015'){
  psplits <- subset(splits, splits$BAT_FLD_CD == 1)
  psplits$B_1B <- with(psplits, B_H - B_2B - B_3B - B_HR)
  table <- table[2, ]
  avg_pitcher_woba <- round(((with(table, (wBB * sum(psplits$B_BB) + (wHBP * sum(psplits$B_HP)) + (w1B * sum(psplits$B_1B)) + (w2B * sum(psplits$B_2B)) + 	(w3B * sum(psplits$B_3B)) + (wHR * sum(psplits$B_HR)))/(sum(psplits$B_AB) + sum(psplits$B_BB) - sum(psplits$B_IBB) + sum(psplits$B_HP) + sum(psplits$B_SH) + sum(psplits$B_SF))))),4)
  return(avg_pitcher_woba)
}
# average_catcher_woba is function to calculate the average catcher wOBA in the league for catchers who have zero plate appearances 
# (same steps as above function)
average_catcher_woba <- function(splits, table = guts_table, year = '2015'){
  csplits <- subset(splits, splits$BAT_FLD_CD == 2)
  csplits$B_1B <- with(csplits, B_H - B_2B - B_3B - B_HR)
  table <- table[2, ]
  avg_catcher_woba <- round(((with(table, (wBB * sum(csplits$B_BB) + (wHBP * sum(csplits$B_HP)) + (w1B * sum(csplits$B_1B)) + (w2B * sum(csplits$B_2B)) + 	(w3B * sum(csplits$B_3B)) + (wHR * sum(csplits$B_HR)))/(sum(csplits$B_AB) + sum(csplits$B_BB) - sum(csplits$B_IBB) + sum(csplits$B_HP) + sum(csplits$B_SH) + sum(csplits$B_SF))))),4)
  return(avg_catcher_woba)
}
# get retrosheet id for a given pitcher (with current team) to get pitcher stats
get_retrosheet_id <- function(pitcher_name, pitcher_team, pitcher_info){
  # find retrosheet id using pitcher's name and current team
  retrosheet_id <- as.character(subset(pitcher_info, retro_name == pitcher_name & mlb_team == pitcher_team)$retro_id)
  return(retrosheet_id)
}
# get run expectancy with various variables as inputs
get_run_expectancy <- function(home_team, batter_team, batter_name, pitcher_name, pitcher_team, year = '2016', pitcher_info, table = guts_table, team_info = teams){
  # get park factors from previous three years (can't use current year's data)
  BPF <- team_info[home_team, 9]
  # get home team, batter team, and pitcher team abbreviations
  home_team <- as.character(team_info[home_team, 1])
  batter_team <- team_info[batter_team, 2]
  pitcher_team <- team_info[pitcher_team, 5]
  #pitching stats -- get L/R splits (wOBA)
  pitcher_retrosheet_id <- as.character(get_retrosheet_id(pitcher_name, pitcher_team, pitcher_info))
  p_splits_R <- subset(pitcher_info, pitcher_info$retro_id == pitcher_retrosheet_id)$Right
  p_splits_L <-subset(pitcher_info, pitcher_info$retro_id == pitcher_retrosheet_id)$Left
  avg <- subset(pitcher_info, pitcher_info$retro_id == pitcher_retrosheet_id)$Average
  # for players with names like "C.J.", need to remove periods to run with stattleship
  name <-  gsub("\\."," ", batter_name)
  name <- gsub("  ", " ", name)
  # if batter's name in in the activePlayers csv, use the given slug name. Otherwise, use the generic "mlb-firstname-lastname"
  player_id <- ifelse(batter_name %in% activePlayers$Name, as.character(subset(activePlayers, batter_team == activePlayers$Team & activePlayers$Name == batter_name)$Slug), tolower(paste('mlb', as.character(gsub(" ", "-", name)), sep ="-")))
  # get stat line (and calculate wOBA) for given player
  stats <- player_wOBA(batter_team, player_id, splits, table, '2016')
  # get current league wOBA and the scale for run value per plate appearance calculation
  league_wOBA <- table[year,1]
  current_scale <- table[year,2]
  # if an empty stat line is returned, use all the averages to calculate run value per plate appearance
  if(nrow(stats) == 0){
    opp_woba <- avg
    avg_woba <- average_woba(splits, table = guts_table, year = '2015')
    RV_Per_PA <- round(((as.numeric(avg_woba) - as.numeric(league_wOBA))/current_scale) * BPF,4)
    return(RV_Per_PA)
  }else{
    # Otherwise use player's stats to calculate their run value per plate appearance and return necessary stats
    stats$opp_wOBA <- ifelse(stats$bats == 'bats_right', p_splits_R, (ifelse(stats$bats =='bats_left', p_splits_L, avg)))
    stats$avg_woba <- with(stats,(woba) - (woba * (woba - opp_wOBA)))
    stats$RV_Per_PA <- round(with(stats,(as.numeric(avg_woba) - as.numeric(lg_woba))/woba_scale) * BPF,4)
    stats <- stats[-c(4:29)]
    return(stats)
  }
}
# main function gets the run expectancy given specific factors
main_function <- function(state, count, batter, stats, env = Run_Env, condition = count_state){
  # calculate the run environment difference to correct the count_state csv values
  avg_run_env <- (sum(env[,1])/(nrow(env)))
  yr_run_env <- env[as.character(year),1]
  dif <- ((yr_run_env-avg_run_env)/avg_run_env)
  cs <- condition[state,count]
  RE_with_Run_Env <- ((cs)*(dif) + (cs))
  # if stats is of type "double", the average values were used, so entire stat line is not returned
  if(typeof(stats) == "double"){
    RE <- round(stats + RE_with_Run_Env, 2)
  }else{
    # otherwise get the RV/PA stat and add with run environment
    RE <- round(as.numeric(stats[, 7]) + RE_with_Run_Env, 2)
  }
  return(RE)
}
# fun1 gets run expectancy using the above function
fun1 <- function(home_team,batter_team,pitcher_team,pitcher_name,state,count,batter_name){
  # don't allow for not valid matchups
  if(batter_team == pitcher_team){
    return('Not a Valid Matchup')
  }else{
    stats <- get_run_expectancy(home_team,batter_team,batter_name, pitcher_name,pitcher_team, year, pitcher_info, guts_table, teams)
    RE <- main_function(state,count,batter_name, stats)
    return(RE)
  }
}

### Win Probability ###

# get percents of singles, doubles, triples, and homeruns out of all player's hits
get_percents <- function(batter_team, batter_name, team_info = teams){
  suppressWarnings(python.load("batterPercentages.py"))
  team <- as.character(team_info[batter_team,8])
  percents <- python.call("get_percentages", batter_name, team)
  return(percents)
} 
# get batting average for specific player
get_ba <- function(batter_team, batter_name, team_info){
  suppressWarnings(python.load("batterBA.py"))
  team <- as.character(team_info[batter_team,8])
  BA <- suppressWarnings(python.call("get_ba", batter_name, team))
  return(BA)
} 
# get league batting average
league_ba <- function(league){
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
pitcher_stats <- function(p_stat, pitcher){
  league <- "mlb"
  sport <- "baseball"
  ep <- "stats"
  # p_stat is a specific stat
  q_body <- list(stat = p_stat, type = 'baseball_pitcher_stat', player_id = pitcher)
  player <- ss_get_result(sport = sport, league = league, ep = ep,
                          query = q_body, version = 1, walk = FALSE)
  players_df <- data.frame(player[[1]]$stats)
  x <- sum(players_df$stat, na.rm = TRUE)
  return(x)
}
# use above function to get all necessary stats to calculate batting average against for pitcher
get_baa <- function(pitcher_player_id, league){
  neededStats <- list('pitcher_hit_by_pitcher','pitcher_hits','pitcher_intentional_walks','pitchers_sacrifice_flys','pitcher_sacrifice_hits','pitcher_walks','batters_faced','catcher_interferences')
  stats <- mapply(pitcher_stats, neededStats, pitcher_player_id)
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
matchup <- function(batter_team, batter_name, pitcher_player_id,wpstates, half_inning, state, count, rdiff, home_team, visiting_team, pitcher_name, pitcher_team, cs, records, team_info, league){
  # get batter's batting average
  x <- as.numeric(get_ba(batter_team, batter_name, team_info))
  # get pitcher's batting average against
  if(league == 'inter'){league <- as.character(teams[pitcher_team, 3])}
  y <- as.numeric(get_baa(pitcher_player_id, league))
  # get league batting average
  if(league == 'inter'){league <- as.character(teams[batter_team, 3])}
  z <- league_ba(league)
  # use log5 formula to calculate probability of a hit for the batter
  exAVG <- ((x*y)/z)/(((x*y)/z) + ((1 -x)*(1-y))/(1-z))
  deltaAVG <- (exAVG - z) + 1
  # get percentages of singles, double, triples, homeruns from number of hits
  batterPercentages <- as.numeric(get_percents(batter_team, batter_name))
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
    out_state <- str_replace(state, '2', '0')
    out_state <- str_replace(state, substr(state,1,3), '000')
    # find the next half inning
    if(substr(half_inning,5,5)=='1'){
      out_half_inning <- paste(as.character(as.numeric(substr(half_inning,1,1))+1), '0')
      out_half_inning <- min(9, out_half_inning)
    }else if(substr(half_inning,5,5)=='0'){
      out_half_inning <- paste(substr(half_inning,1,1), '1')
    }
  }else{
    # otherwise, calculate the new out_state and the half inning would stay the same
    # x is the current number of outs and y is if an out is made
    x <- as.numeric(substr(state, 5, 5))
    y <- x + 1
    out_state <- paste(substr(state,1,4),  as.character(y), sep = "")
    out_half_inning <- half_inning
  } 
  # new state if a single is hit
  s_state <- as.character(subset(deltaState, deltaState$State == state)$Single)
  # new state if a double is hit
  d_state <- as.character(subset(deltaState, deltaState$State == state)$Double)
  # new state if a triple is hit
  t_state <- as.character(subset(deltaState, deltaState$State == state)$Triple)
  # new state if a home run is hit
  hr_state <- as.character(subset(deltaState, deltaState$State == state)$HR)
  # run differential if a single is hit
  s_rdiff <- get_s_rdiff(state, half_inning, rdiff)
  # run differential if a double is hit
  d_rdiff <- get_d_rdiff(state, half_inning, rdiff)
  # run differential if a triple is hit
  t_rdiff <- get_t_rdiff(state, half_inning, rdiff)
  # run differential if a home run is hit
  hr_rdiff <-  get_hr_rdiff(state, half_inning, rdiff)
  # calculate the current Win Probability using WP function
  currentWP <- corrector(half_inning, state, rdiff, count, wpstates, cs)
  # calculate the win probability if a single is hit
  WPsingle <- corrector(half_inning, s_state, s_rdiff, count, wpstates, cs)
  WPsingle[is.na(WPsingle)] <- 0
  # calculate the win probability if a double is hit
  WPdouble <- corrector(half_inning, d_state, d_rdiff, count, wpstates, cs)
  WPdouble[is.na(WPdouble)] <- 0
  # calculate the win probability if a triple is hit
  WPtriple <- corrector(half_inning, t_state, t_rdiff, count, wpstates, cs)
  WPtriple[is.na(WPtriple)] <- 0
  # calculate the win probability if a home run is hit
  WPhr <- corrector(half_inning, hr_state, hr_rdiff, count, wpstates, cs)
  WPtriple[is.na(WPtriple)] <- 0
  # calculate the win probability if an out is made
  WPout <- corrector(half_inning, out_state, rdiff, count, wpstates, cs)
  WPout[is.na(WPout)] <- 0
  # account for ninth inning edge cases
  # if top of the ninth and two outs - win prob out an out is zero
  if(half_inning == '9 0' & substr(state, 5,5) == 2 & rdiff > 0){
    WPout <- 0
  }
  # if bottom of the ninth and a single, double, triple, or hr would cause a walk off win, win-prob of that event is 1
  if(half_inning == '9 1'){
    WPsingle <- ifelse(s_rdiff > 0, 1, WPsingle)
    WPdouble <- ifelse(d_rdiff > 0, 1, WPdouble)
    WPtriple <- ifelse(t_rdiff > 0, 1, WPtriple)
    WPhr <- ifelse(hr_rdiff > 0, 1, WPhr)
  }
  # sum the differences in win probabilities for each scenario multiplied by their corresponding probabilities
  delta <- ((WPout - currentWP)*pout) + ((WPsingle - currentWP)*psingle) + ((WPdouble - currentWP)*pdouble) + ((WPtriple - currentWP)*ptriple) + ((WPhr - currentWP)*phr)
  # return the new win probability given the pitcher/batter calculations
  newWP <- (as.numeric(currentWP*100)) + delta
  return(newWP/100)
}
# calculate run differential if a single is hit using current state
get_s_rdiff <- function(state, half_inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  return(rdiff)
}
# calculate run differential if a double is hit using current state
get_d_rdiff <- function(state, half_inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,2,2) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  return(rdiff)
}
# calculate run differential if a triple is hit using current state
get_t_rdiff <- function(state, half_inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,2,2) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,1,1) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  return(rdiff)
}
# calculate run differential if a homerun is hit using current state
get_hr_rdiff <- function(state, half_inning, rdiff){
  rdiff <- ifelse(substr(state,3,3) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,2,2) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(state,1,1) == 1, ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1), rdiff)
  rdiff <- ifelse(substr(half_inning,3,3) == 0, rdiff - 1, rdiff + 1)
  return(rdiff)
}
# run matchup function using all variables
pitcher_batter_WP <- function(wpstates, half_inning, state, count, rdiff, home_team, visiting_team, pitcher_name, batter_name, pitcher_team, batter_team, cs, records, team_info, league){
  # impossible for home team to be leading in the top of the first or for the game to go to the bottom of the ninth if home team is ahead
  if(rdiff > 0 & (half_inning == '1 0'|half_inning == '9 1')){
    return('Impossible')
  }else{
    # correct for nicknames like "C.J." now for pitchers
    name <-  gsub("\\."," ", pitcher_name)
    name <- gsub("  ", " ", name)
    # get team abbreviation 
    team <- as.character(team_info[pitcher_team, 8])
    # if pitcher is in list of current players, get corresponding slug name, otherwise use generic form of "mlb-firstname-lastname"
    pitcher_player_id <- ifelse(pitcher_name %in% activePlayers$Name, as.character(subset(activePlayers, team == activePlayers$Team & activePlayers$Name == pitcher_name)$Slug), tolower(paste('mlb', as.character(gsub(" ", "-", name)), sep ="-")))
    # run matchup function and return specific win probability given pitcher and batter
    pitcher_batter_matchup <- matchup(batter_team, batter_name, pitcher_player_id, wpstates, half_inning, state, count, rdiff, home_team, visiting_team, pitcher_name, pitcher_team, cs, records, team_info, league)
    # if an extreme edge case and NA is returned despite corrective factors, return baseline value for game situation
    if(is.na(pitcher_batter_matchup == TRUE)){
      pitcher_batter_matchup <- corrector(half_inning, state, rdiff, count, wpstates, cs)
    }
    win_prob <- standings(home_team, visiting_team, teams, records, pitcher_batter_matchup, half_inning)
    return(win_prob) 
  }
}
# run corrective measures for state values
state_correction <- function(half_inning, state, rdiff, wpstates){
  # states in order from lowest win probability to highest win probability
  states <- c('000 2','100 2','000 1','010 2','001 2','110 2','101 2','000 0','100 1','011 2','010 1','111 2','100 0','110 1','001 1','010 0','101 1','011 1','001 0','110 0','111 1','101 0','011 0','111 0')
  # select current permutation, and get all permutations with the other base/out states
  perm <- paste(half_inning, state, rdiff, sep = " ")
  perms <- c(paste(half_inning,states[1],rdiff,sep = " "),paste(half_inning,states[2],rdiff,sep = " "),paste(half_inning,states[3],rdiff,sep = " "),paste(half_inning,states[4],rdiff,sep = " "),paste(half_inning,states[5],rdiff,sep = " "),paste(half_inning,states[6],rdiff,sep = " "),paste(half_inning,states[7],rdiff,sep = " "),paste(half_inning,states[8],rdiff,sep = " "),paste(half_inning,states[9],rdiff,sep = " "),paste(half_inning,states[10],rdiff,sep = " "),
             paste(half_inning,states[11],rdiff,sep = " "),paste(half_inning,states[12],rdiff,sep = " "),paste(half_inning,states[13],rdiff,sep = " "),paste(half_inning,states[14],rdiff,sep = " "),paste(half_inning,states[15],rdiff,sep = " "),paste(half_inning,states[16],rdiff,sep = " "),paste(half_inning,states[17],rdiff,sep = " "),paste(half_inning,states[18],rdiff,sep = " "),paste(half_inning,states[19],rdiff,sep = " "),paste(half_inning,states[20],rdiff,sep = " "),
             paste(half_inning,states[21],rdiff,sep = " "),paste(half_inning,states[22],rdiff,sep = " "),paste(half_inning,states[23],rdiff,sep = " "),paste(half_inning,states[24],rdiff,sep = " "))
  # find the win probability with all of the base/out states
  allstates <- c((subset(wpstates, wpstates$State == perms[1]))$WP,(subset(wpstates, wpstates$State == perms[2]))$WP,(subset(wpstates, wpstates$State == perms[3]))$WP,(subset(wpstates, wpstates$State == perms[4]))$WP,(subset(wpstates, wpstates$State == perms[5]))$WP,(subset(wpstates, wpstates$State == perms[6]))$WP,(subset(wpstates, wpstates$State == perms[7]))$WP,(subset(wpstates, wpstates$State == perms[8]))$WP,(subset(wpstates, wpstates$State == perms[9]))$WP,(subset(wpstates, wpstates$State == perms[10]))$WP,(subset(wpstates, wpstates$State == perms[11]))$WP,(subset(wpstates, wpstates$State == perms[12]))$WP,(subset(wpstates, wpstates$State == perms[13]))$WP,(subset(wpstates, wpstates$State == perms[14]))$WP,(subset(wpstates, wpstates$State == perms[15]))$WP,(subset(wpstates, wpstates$State == perms[16]))$WP,(subset(wpstates, wpstates$State == perms[17]))$WP,(subset(wpstates, wpstates$State == perms[18]))$WP,(subset(wpstates, wpstates$State == perms[19]))$WP,(subset(wpstates, wpstates$State == perms[20]))$WP,(subset(wpstates, wpstates$State == perms[21]))$WP,(subset(wpstates, wpstates$State == perms[22]))$WP,(subset(wpstates, wpstates$State == perms[23]))$WP,(subset(wpstates, wpstates$State == perms[24]))$WP)
  # if top of the inning, home win probability should be going down in better states (decreasing order)
  if(substr(half_inning,3,3) == '0'){
    sorted <- sort(allstates, decreasing = TRUE)
    index <- (as.numeric(which(perms == perm)))
    wpstate <- sorted[index]
  }else if(substr(half_inning,3,3) == '1'){
    # otherwise sort in increasing order
    sorted <- sort(allstates)
    index <- as.numeric(which(perms == perm))
    wpstate <- sorted[index]
  }
  # return values between 0 and 1 only
  ifelse(wpstate >= 1, return(0.9999), return(wpstate))
  ifelse(wpstate <= 0, return(0.0001), return(wpstate))
}
# run corrective measures for run values
run_correction <- function(half_inning, state, rdiff, wpstates){
  # if top of the inning and home team leading:
  if(substr(half_inning,3,3) == '0'){
    if(rdiff > 0){
      # calculate WP for same state in all other innings (not top of first since home team could not be ahead)
      upper <- c(state_correction('2 0', state, rdiff, wpstates), state_correction('3 0', state, rdiff, wpstates),
                 state_correction('4 0', state, rdiff, wpstates), state_correction('5 0', state, rdiff, wpstates), state_correction('6 0', state, rdiff, wpstates),
                 state_correction('7 0', state, rdiff, wpstates), state_correction('8 0', state, rdiff, wpstates), state_correction('9 0', state, rdiff, wpstates))
      # sort in order and select value that is in same order of the basic win probabilities for the base/out states
      index <- as.numeric(substr(half_inning,1,1)) - 1 
      sorted <- sort(upper)
      updated <- sorted[index]
      return(min(updated, 0.9999))
      # top of the inning and visiting team leading:
    }else{
      # calculate WP for same state in all other innings
      upper <- c(state_correction('1 0', state, rdiff, wpstates), state_correction('2 0', state, rdiff, wpstates), state_correction('3 0', state, rdiff, wpstates),
                 state_correction('4 0', state, rdiff, wpstates), state_correction('5 0', state, rdiff, wpstates), state_correction('6 0', state, rdiff, wpstates),
                 state_correction('7 0', state, rdiff, wpstates), state_correction('8 0', state, rdiff, wpstates), state_correction('9 0', state, rdiff, wpstates))
      # sort in order and select value that is in same order of the basic win probabilities for the base/out states
      index <- as.numeric(substr(half_inning,1,1))
      sorted <- sort(upper, decreasing = TRUE)
      updated <- sorted[index]
      return(max(updated, 0.0001))
    }
    # bottom of the inning and home team winning:
  }else if(substr(half_inning,3,3) == '1'){
    if(rdiff > 0){
      # calculate WP for same state in all other innings (not bottom of ninth since game would be over)
      lower <- c(state_correction('1 1', state, rdiff, wpstates), state_correction('2 1', state, rdiff, wpstates), state_correction('3 1', state, rdiff, wpstates),
                 state_correction('4 1', state, rdiff, wpstates), state_correction('5 1', state, rdiff, wpstates), state_correction('6 1', state, rdiff, wpstates),
                 state_correction('7 1', state, rdiff, wpstates), state_correction('8 1', state, rdiff, wpstates))
      # sort in order and select value that is in same order of the basic win probabilities for the base/out states
      index <- as.numeric(substr(half_inning,1,1)) 
      sorted <- sort(lower)
      updated <- sorted[index]
      return(min(updated, 0.9999))
      # bottom of the inning and visiting team winning:
    }else{
      # calculate WP for same state in all other innings
      lower <- c(state_correction('1 1', state, rdiff, wpstates), state_correction('2 1', state, rdiff, wpstates), state_correction('3 1', state, rdiff, wpstates),
                 state_correction('4 1', state, rdiff, wpstates), state_correction('5 1', state, rdiff, wpstates), state_correction('6 1', state, rdiff, wpstates),
                 state_correction('7 1', state, rdiff, wpstates), state_correction('8 1', state, rdiff, wpstates), state_correction('9 1', state, rdiff, wpstates))
      # sort in order and select value that is in same order of the basic win probabilities for the base/out states
      index <- as.numeric(substr(half_inning,1,1))
      sorted <- sort(lower, decreasing = TRUE)
      updated <- sorted[index]
      return(max(updated, 0.0001))
    }
  }
}
# take average of state correction and run correction
corrector <- function(half_inning, state, rdiff, count, wpstates, cs){
  A <- state_correction(half_inning, state, rdiff, wpstates)
  B <- run_correction(half_inning, state, rdiff, wpstates)
  wpstate <- (A+B)/2
  wpstate_plus_one_run <- ifelse(substr(half_inning, 3, 3) == '0', (state_correction(half_inning, state, rdiff - 1, wpstates) + run_correction(half_inning, state, rdiff - 1, wpstates))/2, (state_correction(half_inning, state, rdiff + 1, wpstates) + run_correction(half_inning, state, rdiff + 1, wpstates))/2)
  # calculate the difference in the win probabilities
  state_dif <- (wpstate_plus_one_run - wpstate)
  # find the run expectancy with the current state and count
  re <- cs[state,count]
  # find the general run expectancy just with the base/out state (count back to 0-0) 
  raw_re <- cs[state,2]
  # find the change in the run expectancy
  dif <- (as.numeric(re) - as.numeric(raw_re))/as.numeric(raw_re)
  delta <- state_dif * dif
  # add the current win probability to the change if a run scored
  dynamic_state <- wpstate + delta
  return(dynamic_state)
}
# lastly, factor in teams W-L record for team v team matchup
standings <- function(home_team, visiting_team, team_info, records, wpstate, half_inning){
  hleague <- teams[home_team, 3]
  # get league of away team
  aleague <- teams[visiting_team, 3]
  # get abbreviation of home team
  home_team <- teams[home_team,8]
  # get abbreviation of away team
  visiting_team <- teams[visiting_team,8]
  # if an interleague game, use a different set of calculations
  if(hleague != aleague){
    # get records for both teams (interleague record)
    hrecord <- subset(records, records$Team == home_team & records$Opponent == as.character(aleague))
    arecord <- subset(records, records$Team == visiting_team & records$Opponent == as.character(hleague))
    # if home team or away team hasn't played any games against a team from the other league, use their total record to calculate winning percentage
    if(hrecord$Wins + hrecord$Losses == 0){
      hrecord <- subset(records, records$Team == home_team)
      hpercentage <- sum(hrecord$Wins)/(sum(hrecord$Losses)+sum(hrecord$Wins))
    }
    if(arecord$Wins + arecord$Losses == 0){
      arecord <- subset(records, records$Team == home_team)
      apercentage <- sum(arecord$Wins)/(sum(arecords$Losses)+sum(arecords$Wins))
    }else{
      # otherwise just calculate winning percentage from interleague games
      hpercentage <- as.numeric(hrecord$Wins)/(as.numeric(hrecord$Losses)+as.numeric(hrecord$Wins))
      apercentage <- as.numeric(arecord$Wins)/(as.numeric(arecord$Losses)+as.numeric(arecord$Wins))
    }
    # calculate win probability for home team against the visiting team
    P_ab <- (hpercentage-apercentage)+0.5
  }else if(home_team == visiting_team){
    # if home screen is set (for example Angels v. Angels), return generic win probability
    P_ab <- 0.5
  }else{
    # if game is between teams in the same league, get record of both teams, and calculate total number of games each team has played
    record_a <- subset(records, records$Team == as.character(home_team))
    games_a <- as.numeric(sum(record_a$Wins) + sum(record_a$Losses))
    record_b <- subset(records, records$Team == as.character(visiting_team))
    games_b <- as.numeric(sum(record_b$Wins) + sum(record_b$Losses))
    # if no games have been played by one team, return generic win probability
    if(games_a == 0){
      P_ab <- 0.5
    }else if(games_b == 0){
      P_ab <- 0.5
      # otherwise find win probabilities for each team and use log5 equation for probability between two teams
    }else{
      P_a <- as.numeric(sum(record_a$Wins)/games_a)
      P_b <-as.numeric(sum(record_b$Wins)/games_b)
      P_ab <- (P_a - (P_a * P_b))/(P_a + P_b - (2*(P_a*P_b)))
    }
  }
  # find the amount above or below the basic 50/50 chances
  x <- P_ab - 0.50
  # value the team standings propotionally less as the game continues, and multiply by updated state to get the win probability with the team standings
  wp_with_team_standing <- as.numeric(wpstate)*(1+x/as.numeric(substr(half_inning,1,1)))
  # check that high probability cases do not exceed 1 and low probability cases do not fall below 0
  if(wpstate > 0.90){
    wp_with_team_standing <- min(wp_with_team_standing, 0.9999)
  }else if(wpstate < 0.1){
    wp_with_team_standing <- max(wp_with_team_standing, 0.01)
  }
  bpf <- as.numeric(team_info[home_team, 9])
  if(bpf > 1){
    pf <- wp_with_team_standing * bpf
    dif <- pf - wp_with_team_standing
    wp_team_park <- wp_with_team_standing - dif
  }else{
    wp_team_park <- wp_with_team_standing * bpf
  }
  return(round(wp_team_park,3))
}
probabilities <- function(batter_team, pitcher_team, batter, pitcher, league, state, count){
  suppressWarnings(python.load("probabilities.py"))
  percents <- python.call("get_probabilities", as.character(batter_team), as.character(pitcher_team), as.character(batter), as.character(pitcher), as.character(league), as.character(state), as.character(count))
  return(percents)
}