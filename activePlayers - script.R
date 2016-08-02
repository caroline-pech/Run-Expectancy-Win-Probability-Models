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
get.batters <- function(batter.team, sport = sport, league = league, ep = ep){
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  x <- do.call('rbind', lapply(pls, function(x) x$players))
  y <- subset(x, x$active == TRUE)
  return(y$name)
}
get.slugs <- function(batter.team, sport = sport, league = league, ep = ep){
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  x <- do.call('rbind', lapply(pls, function(x) x$players))
  y <- subset(x, x$active == TRUE)
  return(y$slug)
}
get.position <- function(batter.team, sport = sport, league = league, ep = ep){
  league.team <- paste(league, batter.team, sep = "-")
  q_body <- list(team_id = league.team)
  pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
  x <- do.call('rbind', lapply(pls, function(x) x$players))
  y <- subset(x, x$active == TRUE)
  return(y$position_name)
}
batters <- mapply(get.batters, teams$batter.team, sport, league, ep)
slugs <- mapply(get.slugs, teams$batter.team, sport, league, ep)
position <- mapply(get.position, teams$batter.team, sport, league, ep)
x <- rbind(batters,slugs,position)
dimnames(x)[[2]] <- as.character(teams[,2])

get_df <- function(i){
  a <- cbind(data.frame(c(x[1,i],x[2,i],x[3,i])), as.character(teams[i,2]))
  dimnames(a)[[2]] <- c('Name','Slug','Position','Team')
  return(a)
}
info <- rbind(get_df(1), get_df(2),get_df(3), get_df(4),get_df(5),get_df(6),get_df(7),get_df(8),get_df(9),get_df(10),
              get_df(11), get_df(12),get_df(13), get_df(14),get_df(15),get_df(16),get_df(17),get_df(18),get_df(19),get_df(20),
              get_df(21), get_df(22),get_df(23), get_df(24),get_df(25),get_df(26),get_df(27),get_df(28),get_df(29),get_df(30))

write.csv(info, "activePlayers.csv")