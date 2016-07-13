library(dplyr)
library(retrosheet)
library(rvest)
library(stringr)
library(rPython)
setwd("~/Desktop/run-expectancy")
teams <- read.csv("teams2016.csv", header = TRUE)
dimnames(teams)[[1]] <- teams$X
teams$X <- NULL
python.load("ALstandings.py")
python.load("NLstandings.py")
AL.records <- read.csv(python.call("get_AL_standings"), header = TRUE)
NL.records <- read.csv(python.call("get_NL_standings"), header = TRUE)
records <- rbind(AL.records, NL.records)
game.logs <- read.csv("gamelogs.merged.csv", header = FALSE)[,1:12]
headers <- read.csv("fields2.csv")
names(game.logs) <- headers$Header
splits <- read.csv("splits.csv", header = TRUE)
year <- '2016' 
pitcherSplits <- read.csv("pitcherData-splits.csv", header = TRUE)
dimnames(pitcherSplits)[[1]] <- pitcherSplits$X
pitcherSplits$X <- NULL
pitcherData <- read.csv("pitcherData2016.csv", header=TRUE)
dimnames(pitcherData)[[1]] <- pitcherData$X
pitcherData$X <- NULL
count.state <- read.csv("1990.2015.RE.States.Count.csv")
dimnames(count.state)[[1]] <- count.state[, 1]
wpstates <- read.csv("WPstates.csv", header = TRUE)
dimnames(wpstates)[[2]] <- c("State","WP")
guts_table <- read.csv("guts_table_2016.csv", header = TRUE)
dimnames(guts_table)[[1]] <- guts_table$X
guts_table$X <- NULL
Run.Env <- read.csv("Run.Environments.csv", header = TRUE)
batters <- read.csv("batters.csv", header = TRUE)
batters <- batters[c(2,3,4)]

################################
get.BPF <- function(year, NT, home.team){
  home.team <- as.character(teams[(home.team), 1])
  game.logs.home <- subset(game.logs, game.logs$HOME_TEAM == home.team & substr(game.logs$DATE,1,4) == as.character(year) & game.logs$VISITING_LEAGUE==game.logs$HOME_LEAGUE)
  game.logs.away <- subset(game.logs, game.logs$VISITING_TEAM == home.team & substr(game.logs$DATE,1,4) == as.character(year) & game.logs$VISITING_LEAGUE==game.logs$HOME_LEAGUE)
  homeleague <- ifelse(game.logs.home[1,8] == 'AL', 1, 0)
  homeRS <- sum(game.logs.home$HOME_SCORE + game.logs.home$VISITING_SCORE)
  home.games <- nrow(game.logs.home)
  avg.home.RS <- homeRS/home.games
  awayRS <-  sum(game.logs.away$HOME_SCORE + game.logs.away$VISITING_SCORE)
  away.games <- nrow(game.logs.away)
  avg.away.RS <- awayRS/away.games
  PF <- avg.home.RS/avg.away.RS
  WAH <- sum(ifelse(game.logs.home$HOME_SCORE > game.logs.home$VISITING_SCORE,1,0))
  LOR <- sum(ifelse(game.logs.away$HOME_SCORE > game.logs.away$VISITING_SCORE,1,0))
  IPC <- (18.5 - (WAH/home.games))/(18.5 - (LOR/away.games))
  Run.Factor <- PF/IPC
  OPC <- NT/(NT-1 + Run.Factor)
  SF <- Run.Factor * OPC
  SF1 <- 1 - abs((SF - 1)/NT)
  RHT <- sum(game.logs.home$HOME_SCORE)/home.games
  RAT <- sum(game.logs.away$VISITING_SCORE)/away.games
  OHT <- sum(game.logs.home$VISITING_SCORE)/home.games
  OAT <- sum(game.logs.away$HOME_SCORE)/away.games
  all.games <- subset(game.logs, substr(game.logs$DATE,1,4) == as.character(year) & game.logs$VISITING_LEAGUE==game.logs$HOME_LEAGUE)
  all.games$OVERALL_LEAGUE <- ifelse(all.games$HOME_LEAGUE=='AL', 1, 0)
  all.league.games <- subset(all.games, all.games$OVERALL_LEAGUE==homeleague)
  RAL <- sum(all.league.games$HOME_SCORE + all.league.games$VISITING_SCORE)/nrow(all.league.games)
  rawTPR <- 1
  TBR <- (abs(RAT/SF1 + RHT/SF)*abs(1 + (rawTPR - 1)/(NT - 1)))/RAL
  TPR <- (abs(OAT/SF1 + OHT/SF)*abs(1 + (TBR - 1)/(NT - 1)))/RAL
  BPF <- (SF + SF1)/(2 * abs(1 + (TPR - 1)/(NT - 1)))
  return(BPF)
}

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
  NT <- sum(teams$league == teams[as.character(home.team),3])
  BPF <- mean(mapply(get.BPF, years, NT, home.team))
  return(BPF)
}

# Batter Analysis

#calculates wOBA for entire roster
roster.wOBA <- function(batter.team, player_id, splits, table = guts_table, year = '2016'){
  # library("devtools")
  # devtools::install_github("stattleship/stattleship-r")
  # library(stattleshipR)
  source("zzz.R")
  source("set_token.R")
  source("ss_get_result.R")
  set_token("bf3c65fd3952ea434f4a96b641744475")
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
  if(is.null(game_logs$player_id)){
    roster <- data.frame()
    return(roster)
  }else{
    game_logs <- merge(players, game_logs, by='player_id') 
    batter.stats <-  
      game_logs %>%
      filter(game_played == TRUE | batters_faced > 0) %>%
      group_by(name, bats, position_abbreviation) %>%
      summarise(PA = (sum(at_bats)+sum(walks)+sum(hit_by_pitch)+sum(sacrifice_flys)+sum(sacrifice_hits)), BB = sum(walks), HBP = sum(hit_by_pitch), singles = sum(singles), doubles = sum(doubles), triples = sum(triples), hr = sum(home_runs), SB = sum(stolen_bases), CS = sum(caught_stealing), AB = sum(at_bats), IBB = sum(intentional_walks_against), SF = sum(sacrifice_flys), SH = sum(sacrifice_hits)) 
    bdata <- data.frame(batter.stats)
    season <- table[year, ]
    bdata <- merge(bdata, season)
    bdata$regress <- ifelse(bdata$PA < 220 & bdata$PA > 0, TRUE, FALSE)
    bdata$woba <- ifelse(bdata$PA != 0,round(((with(bdata, (wBB * (BB) + (wHBP * (HBP)) + (w1B * (singles)) + (w2B * (doubles)) + (w3B * (triples)) + (wHR * (hr)))/((AB) + (BB) - (IBB) + (HBP) + (SH) + (SF))))),4), (ifelse(bdata$position_abbreviation =='RP'|bdata$position_abbreviation =='SP', (average.pitcher.woba(splits, guts_table, (as.numeric(year)-1))), ifelse(bdata$position_abbreviation == 'C', (average.catcher.woba(splits, guts_table, (as.numeric(year)-1))),(average.woba(splits, guts_table, (as.numeric(year)-1)))))))
    regress.data <- subset(bdata, bdata$regress == TRUE)
    bdata <- subset(bdata, bdata$regress != TRUE)
    regress.data$z <- as.numeric(regress.data$woba)/as.numeric(regress.data$PA)
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
    bdata <- rbind(bdata, pitchers, catchers, first.base,second.base,third.base,SS,LF,CF,RF,DH)
    dimnames(bdata)[[1]] <- c(bdata$name)
    bdata$name <- NULL
    return(bdata)
  }
}

get.coeffs <- function(defensive.position, splits, table, year = '2016'){
  psplits <- subset(splits, splits$BAT_FLD_CD == defensive.position & splits$YEAR == as.numeric(year) - 1)
  psplits$B_1B <- with(psplits, B_H - B_2B - B_3B - B_HR)
  table <-table[as.character(as.numeric(year) - 1), ]
  psplits$woba <- round(((with(table, (wBB * (psplits$B_BB) + (wHBP * (psplits$B_HP)) + (w1B * (psplits$B_1B)) + (w2B * (psplits$B_2B)) + 	(w3B * (psplits$B_3B)) + (wHR * (psplits$B_HR)))/((psplits$B_AB) + (psplits$B_BB) - (psplits$B_IBB) + (psplits$B_HP) + (psplits$B_SH) + (psplits$B_SF))))),4)
  psplits$z <- psplits$woba/psplits$B_PA
  coeff <- summary(lm(psplits$woba ~ psplits$z))$coefficients
  return(c(coeff[1,1], coeff[2,1]))
}
new.data <- function(position.data, defensive.position, table = guts_table, year = '2016', splits){
  position <- get.coeffs(defensive.position, splits, guts_table, '2016')
  position.data$woba <- position[1] + position[2]*as.numeric(position.data$woba)/position.data$PA
  position.data$z <- NULL
  position.data$retrosheet.id <- NULL
  return(position.data)
}
average.woba <- function(splits, table = guts_table, year = '2015'){
  osplits <- subset(splits, splits$BAT_FLD_CD != 1 & splits$BAT_FLD_CD != 2)
  osplits$B_1B <- with(osplits, B_H - B_2B - B_3B - B_HR)
  table <-table[as.character(as.numeric(year) - 1), ]
  osplits$woba <- round(((with(table, (wBB * (osplits$B_BB) + (wHBP * (osplits$B_HP)) + (w1B * (osplits$B_1B)) + (w2B * (osplits$B_2B)) + 	(w3B * (osplits$B_3B)) + (wHR * (osplits$B_HR)))/((osplits$B_AB) + (osplits$B_BB) - (osplits$B_IBB) + (osplits$B_HP) + (osplits$B_SH) + (osplits$B_SF))))),4)
  osplits[osplits$woba == "Inf",]$woba = 0
  return(mean(osplits$woba, na.rm = TRUE))
}
average.pitcher.woba <- function(splits, table = guts_table, year = '2015'){
  psplits <- subset(splits, splits$BAT_FLD_CD == 1)
  psplits$B_1B <- with(psplits, B_H - B_2B - B_3B - B_HR)
  table <- table[2, ]
  avg.pitcher.woba <- round(((with(table, (wBB * sum(psplits$B_BB) + (wHBP * sum(psplits$B_HP)) + (w1B * sum(psplits$B_1B)) + (w2B * sum(psplits$B_2B)) + 	(w3B * sum(psplits$B_3B)) + (wHR * sum(psplits$B_HR)))/(sum(psplits$B_AB) + sum(psplits$B_BB) - sum(psplits$B_IBB) + sum(psplits$B_HP) + sum(psplits$B_SH) + sum(psplits$B_SF))))),4)
  return(avg.pitcher.woba)
}
average.catcher.woba <- function(splits, table = guts_table, year = '2015'){
  csplits <- subset(splits, splits$BAT_FLD_CD == 2)
  csplits$B_1B <- with(csplits, B_H - B_2B - B_3B - B_HR)
  table <- table[2, ]
  avg.catcher.woba <- round(((with(table, (wBB * sum(csplits$B_BB) + (wHBP * sum(csplits$B_HP)) + (w1B * sum(csplits$B_1B)) + (w2B * sum(csplits$B_2B)) + 	(w3B * sum(csplits$B_3B)) + (wHR * sum(csplits$B_HR)))/(sum(csplits$B_AB) + sum(csplits$B_BB) - sum(csplits$B_IBB) + sum(csplits$B_HP) + sum(csplits$B_SH) + sum(csplits$B_SF))))),4)
  return(avg.catcher.woba)
}

get.retrosheet.id <- function(pitcher.name, pitcher.team, pitcherData){
  info <- paste(pitcher.team, pitcher.name)
  retrosheet.id <- pitcherData[info, 3]
  return(retrosheet.id)
}

get.run.expectancy <- function(home.team, batter.team, batter.name, pitcher.name, pitcher.team, year = '2016', psplits = pitcherSplits, table = guts_table, team.info = teams){
  pfyear <- as.numeric(year) - 1
  BPF <- park.factors(home.team, pfyear, team.info, game.logs)
  home.team <- as.character(team.info[home.team, 1])
  batter.team <- team.info[batter.team, 2]
  pitcher.team <- team.info[pitcher.team, 5]
  #pitching stats -- splits
  pitcher.retrosheet.id <- as.character(get.retrosheet.id(pitcher.name, pitcher.team, pitcherData))
  p.splits <- psplits[, pitcher.retrosheet.id]
  p.splits.R <- p.splits[1]
  p.splits.L <- p.splits[2]
  avg <- p.splits[3]
  #team batting stats
  name <-  gsub("\\."," ", batter.name)
  name <- gsub("  ", " ", name)
  player_id = tolower(paste('mlb', as.character(gsub(" ", "-", name)), sep ="-"))
  roster <- roster.wOBA(batter.team, player_id, splits, table, '2016')
  league.wOBA <- table[year,1]
  current.scale <- table[year,2]
  if(nrow(roster) == 0){
    opp.woba <- avg
    avg.woba <- average.woba(splits, table = guts_table, year = '2015')
    RV.Per.PA <- round(((as.numeric(avg.woba) - as.numeric(league.wOBA))/current.scale) * BPF,4)
    return(RV.Per.PA)
  }else{
    roster$opp.wOBA <- ifelse(roster$bats == 'bats_right', p.splits.R, (ifelse(roster$bats =='bats_left', p.splits.L, avg)))
    roster$avg.woba <- with(roster,(woba) - (woba * (woba - opp.wOBA)))
    roster$RV.Per.PA <- round(with(roster,(as.numeric(avg.woba) - as.numeric(lg_woba))/woba_scale) * BPF,4)
    roster <- roster[-c(4:29)]
    return(roster)
  }
}

main.function <- function(state, count, batter, stats, env = Run.Env, condition = count.state){
  avg.run.env <- (sum(env[,2])/(nrow(env)))
  yr.run.env <- env[as.character(year),2]
  dif <- ((yr.run.env-avg.run.env)/avg.run.env)
  cs <- condition[state,count]
  RE.with.Run.Env <- ((cs)*(dif) + (cs))
  if(typeof(stats) == "double"){
    RE <- round(stats + RE.with.Run.Env, 2)
  }else{
    RE <- round(as.numeric(stats[, 7]) + RE.with.Run.Env, 2)
  }
  return(RE)
}

fun1 <- function(home.team,batter.team,pitcher.team,pitcher.name,state,count,batter.name){
  if(batter.team == pitcher.team){
    return('Not a Valid Matchup')
  }else{
    stats <- get.run.expectancy(home.team,batter.team,batter.name, pitcher.name,pitcher.team)
    RE <- main.function(state,count,batter.name, stats)
    return(RE)
  }
}

WP <- function(wpstates, half.inning, state, count, rdiff, home.team, visiting.team, cs, records){
  perm <- paste(half.inning, state, rdiff, sep = " ")
  rdiff2 <- ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1)
  perm2 <- paste(half.inning, state, rdiff2, sep = " ")
  wpstate <- (subset(wpstates, wpstates$State == perm))$WP
  wpstate.plus.one.run <- (subset(wpstates, wpstates$State == perm2))$WP
  state.dif <- (wpstate.plus.one.run - wpstate)
  re <- cs[state,count]
  raw.re <- cs[state,2]
  dif <- (as.numeric(re) - as.numeric(raw.re))/as.numeric(raw.re)
  new.wp <- dif * wpstate.plus.one.run
  delta <- state.dif * dif
  dynamic.state <- wpstate + delta
  # adjust if interleague game
  hleague <- teams[home.team, 3]
  aleague <- teams[visiting.team, 3]
  home.team <- teams[home.team,8]
  visiting.team <- teams[visiting.team,8]
  if(hleague != aleague){
    hrecord <- subset(records, records$Team == home.team & records$Opponent == as.character(aleague))
    arecord <- subset(records, records$Team == visiting.team & records$Opponent == as.character(hleague))
    if(hrecord$Wins + hrecord$Losses == 0){
      hrecord <- subset(records, records$Team == home.team)
      hpercentage <- sum(hrecord$Wins)/(sum(hrecord$Losses)+sum(hrecord$Wins))
    }
    if(arecord$Wins + arecord$Losses == 0){
      arecord <- subset(records, records$Team == home.team)
      apercentage <- sum(arecord$Wins)/(sum(arecords$Losses)+sum(arecords$Wins))
    }else{
      hpercentage <- as.numeric(hrecord$Wins)/(as.numeric(hrecord$Losses)+as.numeric(hrecord$Wins))
      apercentage <- as.numeric(arecord$Wins)/(as.numeric(arecord$Losses)+as.numeric(arecord$Wins))
    }
    percentage <- (hpercentage-apercentage)+0.5
  }else if(home.team == visiting.team){
    percentage <- 0.5
  }else{
    record <- subset(records, records$Team == as.character(home.team) & records$Opponent == as.character(visiting.team))
    if(record$Wins + record$Losses == 0){
      percentage <- 0.5
    }else{
      percentage <- as.numeric(record$Wins)/(as.numeric(record$Losses)+as.numeric(record$Wins)) 
    }
  }
  x <- percentage - 0.50
  wp.with.team.standing <- dynamic.state*(1+x/as.numeric(substr(half.inning,1,1)))
  bound <- min(100, wp.with.team.standing)
  return(bound)
}

