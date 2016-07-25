teams <- read.csv("teams2016.csv", header = TRUE)
dimnames(teams)[[1]] <- teams$X
# remove counting column
teams$X <- NULL
game.logs <- read.csv("gamelogs.merged.csv", header = FALSE)[,1:12]
headers <- read.csv("fields2.csv")
names(game.logs) <- headers$Header

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

pfyear <- 2015
BPF <- c(park.factors(dimnames(teams)[[1]][1], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][2], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][3], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][4], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][5], pfyear, teams, game.logs),
         park.factors(dimnames(teams)[[1]][6], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][7], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][8], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][9], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][10], pfyear, teams, game.logs),
         park.factors(dimnames(teams)[[1]][11], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][12], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][13], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][14], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][15], pfyear, teams, game.logs),
         park.factors(dimnames(teams)[[1]][16], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][17], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][18], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][19], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][20], pfyear, teams, game.logs),
         park.factors(dimnames(teams)[[1]][21], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][22], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][23], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][24], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][25], pfyear, teams, game.logs),
         park.factors(dimnames(teams)[[1]][26], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][27], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][28], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][29], pfyear, teams, game.logs),park.factors(dimnames(teams)[[1]][30], pfyear, teams, game.logs))
teams$BPF <- BPF

write.csv(teams, "teams2016.csv")