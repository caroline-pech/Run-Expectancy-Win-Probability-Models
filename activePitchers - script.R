devtools::install_github("stattleship/stattleship-r")
source("zzz.R")
source("set_token.R")
source("ss_get_result.R")
# API given by stattleship site
set_token("bf3c65fd3952ea434f4a96b641744475")
# set parameters for stattleship search
sport <- 'baseball'
league <- 'mlb'
ep <- 'players'
get.pitchers <- function(batter.team, sport = sport, league = league, ep = ep){
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  x <- do.call('rbind', lapply(pls, function(x) x$players))
  y <- subset(x, x$active == TRUE & x$position_name == 'Reliever' | x$position_name == 'Starter')
  return(y$name)
}
get.slugs <- function(batter.team, sport = sport, league = league, ep = ep){
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  x <- do.call('rbind', lapply(pls, function(x) x$players))
  y <- subset(x, x$active == TRUE & x$position_name == 'Reliever' | x$position_name == 'Starter')
  return(y$slug)
}
get.position <- function(batter.team, sport = sport, league = league, ep = ep){
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  x <- do.call('rbind', lapply(pls, function(x) x$players))
  y <- subset(x, x$active == TRUE & x$position_name == 'Reliever' | x$position_name == 'Starter')
  return(y$position_name)
}
pitchers <- mapply(get.pitchers, teams$batter.team, sport, league, ep)
slugs <- mapply(get.slugs, teams$batter.team, sport, league, ep)
positions <- mapply(get.position, teams$batter.team, sport, league, ep)
x <- rbind(pitchers,slugs,positions)
dimnames(x)[[2]] <- as.character(teams[,2])

get_df <- function(i){
  a <- cbind(data.frame(c(x[1,i],x[2,i], x[3,i])), as.character(teams[i,2]))
  dimnames(a)[[2]] <- c('Name','Slug','Position','Team')
  return(a)
}
info <- rbind(get_df(1), get_df(2),get_df(3), get_df(4),get_df(5),get_df(6),get_df(7),get_df(8),get_df(9),get_df(10),
              get_df(11), get_df(12),get_df(13), get_df(14),get_df(15),get_df(16),get_df(17),get_df(18),get_df(19),get_df(20),
              get_df(21), get_df(22),get_df(23), get_df(24),get_df(25),get_df(26),get_df(27),get_df(28),get_df(29),get_df(30))
write.csv(info, 'activePitchers.csv')


x <- t(pitcherSplits)
x <- data.frame(x)
x$retro_id <- dimnames(x)[[1]]
y <- merge(x, pitcherData, by = 'retro_id')
dimnames(y)[[1]] <- paste(y$mlb_team, y$retro_name, sep = " ")

write.csv(y, 'pitcher.info.csv')


pitcherData <- read.csv("pitcherData2016.csv", header = TRUE)


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

pitcherData$retrosheetID <- mapply(get.retrosheet.id, pitcherData$retro_name)

