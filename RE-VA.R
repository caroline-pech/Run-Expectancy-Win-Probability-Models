A10 <- read.csv("2010A.csv", header=FALSE)
B10 <- read.csv("2010B.csv", header=FALSE)
A11 <- read.csv("2011A.csv", header=FALSE)
B11 <- read.csv("2011B.csv", header=FALSE)
A12 <- read.csv("2012A.csv", header=FALSE)
B12 <- read.csv("2012B.csv", header=FALSE)
A13 <- read.csv("2013A.csv", header=FALSE)
B13 <- read.csv("2013B.csv", header=FALSE)
A14 <- read.csv("2014A.csv", header=FALSE)
B14 <- read.csv("2014B.csv", header=FALSE)
A15 <- read.csv("2015A.csv", header=FALSE)
B15 <- read.csv("2015B.csv", header=FALSE)
decadedata <- rbind.data.frame(A10,B10,A11,B11,A12,B12,A13,B13,A14,B14,A15,B15)
headers <- read.csv("fields.csv")
names(decadedata) <- headers$Header
library(dplyr)
library(retrosheet)
library(rvest)
library(stringr)

#################################################

game.logs <- read.csv("gamelogs.merged.csv", header = FALSE)[,1:12]
headers <- read.csv("fields2.csv")
names(game.logs) <- headers$Header
decadedata$HOME.TEAM <- with(decadedata, substr(decadedata$GAME_ID, 1, 3))
splits <- read.csv("splits.csv", header = TRUE)
year <- '2016' 
#change this as necessary
teams <- data.frame(getTeamIDs(as.numeric(year)-1))
teams$batter.team <- (c('laa','bal','bos','chw','cle','det','hou','kc','min','nyy','oak','sea','tb', 'tex','tor','ari','atl','chc','cin','col','la','mia','mil','nym','phi','pit','sd','sf','stl','was'))
teams$league <- (c('AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL'))
teams$full.name <- (c('Los Angeles Angels of Anaheim','Baltimore Orioles','Boston Red Sox','Chicago White Sox','Cleveland Indians','Detriot Tigers','Houston Astros','Kansas City Royals','Minnesota Twins','New York Yankees','Oakland Athletics','Seattle Mariners','Tampa Bay Rays','Texas Rangers','Toronto Blue Jays','Arizona Diamondbacks','Atlanta Braves','Chicago Cubs','Cincinnati Reds','Colorado Rockies','Los Angeles Dodgers','Miami Marlins','Milwaukee Brewers','New York Mets','Philadelphia Phillies','Pittsburgh Pirates','San Diego Padres','San Francisco Giants','St. Louis Cardinals','Washington Nationals'))
teams$pitching.data.team <- (c('LAA','BAL','BOS','CWS','CLE','DET','HOU','KC','MIN','NYY','OAK','SEA','TB','TEX','TOR','ARI','ATL','CHC','CIN','COL','LAD','MIA','MIL','NYM','PHI','PIT','SD','SF','STL','WSH'))
teams$field <- (c('Angel Stadium of Anaheim','Oriole Park at Camden Yards','Fenway Park II','U.S. Cellular Field','Progressive Field','Comerica Park','Minute Maid Park','Kauffman Stadium','Target Field','Yankee Stadium III','O.co Coliseum','Safeco Field','Tropicana Field','Rangers Ballpark in Arlington','Rogers Centre','Chase Field','Turner Field','Wrigley Field','Great American Ball Park','Coors Field','Dodger Stadium','Marlins Park','Miller Park','Citi Field','Citizens Bank Park','PNC Park','Petco Park','AT&T Park','Busch Stadium III','Nationals Park'))
teams$built.year <- (c(2012,1992,1934,2003,2012,2000,2002,1993,2010,2009,2012,1999,1998,2007,2005,2006,1997,1914,2003,1995,1962,2012,2001,2009,2004,2001,2004,2006,2006,2008))
dimnames(teams)[[2]][1] <- ('home.team')
pitcherData <- read.csv('pitcherData.csv', header = TRUE)
pitcherData <- subset(pitcherData, pitcherData$mlb_pos == 'P')
pitcherData <- pitcherData[c(4,14,24,25,26)]
pitcherData <- pitcherData[-which(pitcherData$retro_id == ""), ]
dimnames(pitcherData)[[1]] = paste(pitcherData$mlb_team, pitcherData$retro_name)
count.state <- read.csv("1990.2015.RE.States.Count.csv")
dimnames(count.state)[[1]] <- count.state[, 1]
guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
guts_table <- as.data.frame(guts_table)[-(1:2), (1:14)]
names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
for(i in c(2:ncol(guts_table))) {
  guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
}
dimnames(guts_table)[[1]] <- guts_table$season
guts_table$season <- NULL
Run.Env <- read.csv("Run.Environments.csv", header = TRUE)


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
roster.wOBA <- function(batter.team, splits, table = guts_table, year = '2016'){
  library("devtools")
  devtools::install_github("stattleship/stattleship-r")
  library(stattleshipR)
  set_token("bf3c65fd3952ea434f4a96b641744475")
  sport <- 'baseball'
  league <- 'mlb'
  ep <- 'game_logs'
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id=league.team, status='ended', interval_type='regularseason')
  gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  ep <- 'players'
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE) 
  players<-do.call('rbind', lapply(pls, function(x) x$players))  
  colnames(players)[1] <- 'player_id' 
  game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs))  
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

split.data <- function(pitcher.retrosheet.id, table=guts_table, year = '2016', data = decadedata){
  data$season <- with(data, substr(GAME_ID, 4, 7))
  LHB <- subset(data, PIT_ID == pitcher.retrosheet.id & BAT_HAND_CD == 'L')
  RHB <- subset(data, PIT_ID == pitcher.retrosheet.id & BAT_HAND_CD == 'R')
  season <- table[year, ]
  RH.PA <- nrow(RHB)
  LH.PA <- nrow(LHB)
  
  RHB$SINGLES <- ifelse(RHB$EVENT_CD == 20, 1, 0)
  RHB$DOUBLES <- ifelse(RHB$EVENT_CD == 21, 1, 0)
  RHB$TRIPLES <- ifelse(RHB$EVENT_CD == 22, 1, 0)
  RHB$HRS <- ifelse(RHB$EVENT_CD == 23, 1, 0)
  RHB$BB <- ifelse(RHB$EVENT_CD == 14, 1, 0)
  RHB$IBB <- ifelse(RHB$EVENT_CD == 15, 1, 0)
  RHB$SAC <- ifelse(RHB$SH_FL == TRUE|RHB$SF_FL == TRUE, 1, 0)
  RHB$AB <- ifelse(RHB$EVENT_CD == 6|RHB$EVENT_CD == 8|RHB$EVENT_CD == 14|RHB$EVENT_CD == 15|RHB$EVENT_CD == 16|RHB$EVENT_CD == 17|RHB$SH_FL == TRUE|RHB$SF_FL == TRUE, 0, 1)
  pitcher.R.wOBA <- round(((with(season, (wBB * sum(RHB$BB) + (wHBP * sum(RHB$HBP)) + (w1B * sum(RHB$SINGLES)) + (w2B * sum(RHB$DOUBLES)) + 	(w3B * sum(RHB$TRIPLES)) + (wHR * sum(RHB$HRS)))/(sum(RHB$AB) + sum(RHB$BB) - sum(RHB$IBB) + sum(RHB$HBP) + sum(RHB$SAC))))),4)
  
  LHB$SINGLES <- ifelse(LHB$EVENT_CD == 20, 1, 0)
  LHB$DOUBLES <- ifelse(LHB$EVENT_CD == 21, 1, 0)
  LHB$TRIPLES <- ifelse(LHB$EVENT_CD == 22, 1, 0)
  LHB$HRS <- ifelse(LHB$EVENT_CD == 23, 1, 0)
    
  LHB$BB <- ifelse(LHB$EVENT_CD == 14, 1, 0)
  LHB$IBB <- ifelse(LHB$EVENT_CD == 15, 1, 0)
  LHB$SAC <- ifelse(LHB$SH_FL == TRUE|LHB$SF_FL == TRUE, 1, 0)
  LHB$AB <- ifelse(LHB$EVENT_CD == 6|LHB$EVENT_CD == 8|LHB$EVENT_CD == 14|LHB$EVENT_CD == 15|LHB$EVENT_CD == 16|LHB$EVENT_CD == 17|LHB$SH_FL == TRUE|LHB$SF_FL == TRUE, 0, 1)
  pitcher.L.wOBA <- round(((with(season, (wBB * sum(LHB$BB) + (wHBP * sum(LHB$HBP)) + (w1B * sum(LHB$SINGLES)) + (w2B * sum(LHB$DOUBLES)) + 	(w3B * sum(LHB$TRIPLES)) + (wHR * sum(LHB$HRS)))/(sum(LHB$AB) + sum(LHB$BB) - sum(LHB$IBB) + sum(LHB$HBP) + sum(LHB$SAC))))),4)
  avg <- (pitcher.R.wOBA + pitcher.L.wOBA)/2
  if (RH.PA < 500 & LH.PA > 500){
    return(c(0,pitcher.L.wOBA,avg))
  }  
  if (LH.PA < 500 & RH.PA > 500){
    return(c(pitcher.R.wOBA, 0, avg))
  }
  if (RH.PA >= 500 & LH.PA >= 500){
    return(c(pitcher.R.wOBA,pitcher.L.wOBA, avg))
  }
  if (RH.PA < 500 & LH.PA < 500){
    return(c(0,0,avg))
  }
}

get.retrosheet.id <- function(pitcher.name, pitcher.team, pitcherData){
  info <- paste(pitcher.team, pitcher.name)
  retrosheet.id <- pitcherData[info, 3]
  return(retrosheet.id)
}

get.run.expectancy <- function(home.team, batter.team, pitcher.name, pitcher.team, year = '2016', data = decadedata, table = guts_table, team.info = teams){
  pfyear <- as.numeric(year) - 1
  BPF <- park.factors(home.team, pfyear, team.info, game.logs)
  home.team <- as.character(team.info[home.team, 1])
  batter.team <- team.info[batter.team, 2]
  pitcher.team <- team.info[pitcher.team, 5]
  #pitching stats -- splits
  pitcher.retrosheet.id <- as.character(get.retrosheet.id(pitcher.name, pitcher.team, pitcherData))
  p.splits <- split.data(pitcher.retrosheet.id, table, year, decadedata)
  p.splits.R <- p.splits[1]
  p.splits.L <- p.splits[2]
  avg <- p.splits[3]
  #team batting stats
  roster <- roster.wOBA(batter.team, splits, table, '2016')
  roster$opp.wOBA <- ifelse(roster$bats == 'bats_right', p.splits.R, (ifelse(roster$bats =='bats_left', p.splits.L, avg)))
  roster$avg.woba <- with(roster,(woba) - (woba * (woba - opp.wOBA)))
  # roster$avg.woba <- with(roster, (as.numeric(woba) + as.numeric(opp.wOBA))/2)
  league.wOBA <- table[year,1]
  current.scale <- table[year,2]
  roster$RV.Per.PA <- round(with(roster,(as.numeric(avg.woba) - as.numeric(lg_woba))/woba_scale) * BPF,4)
  roster <- roster[-c(4:29)]
  return(roster)
}

main.function <- function(state, count, batter, roster, env = Run.Env, condition = count.state){
  avg.run.env <- (sum(env[,2])/(nrow(env)))
  yr.run.env <- env[as.character(year),2]
  dif <- ((yr.run.env-avg.run.env)/avg.run.env)
  cs <- condition[state,count]
  RE.with.Run.Env <- ((cs)*(dif) + (cs))
  RE <- round(as.numeric(roster[batter, 7]) + RE.with.Run.Env, 2)
  return(RE)
}

fun1 <- function(home.team,batter.team,pitcher.team,pitcher.name,state,count,batter.name){
  roster <- get.run.expectancy(home.team,batter.team,pitcher.name,pitcher.team)
  RE <- main.function(state,count,batter.name, roster)
  return(RE)
}

