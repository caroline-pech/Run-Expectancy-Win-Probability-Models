error.correction <- function(homeWP, awayWP, count, half.inning, rdiff, state, home.team, visiting.team, cs, records, team.info, wpstates){
  homeWP <- homeWP/100
  awayWP <- awayWP/100
  if(rdiff < 0){
    if(substr(half.inning,3,3) == '0'){
      curWP <- awayWP
      if(curWP>0.92){
        base <- 1 - WP(wpstates, half.inning, state, count, rdiff + 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base >= 1){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff + 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base >= 1){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff + 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base >= 1){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff + 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        awayWP <- (base + change.rdiff(half.inning,state,wpstates)/3 + curWP)/2 * 100
        print(round(awayWP,3))
        return(100 - awayWP)
      }
      else if(curWP < 0.08){
        base <- WP(wpstates, half.inning, state, count, rdiff + 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff + 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff + 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff + 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        awayWP <- (base/x + curWP)/2 * 100
        print(round(awayWP,3))
        return(100 - awayWP)
      }else{
        return(100 - curWP)
      }
    }else if(substr(half.inning,3,3) == '1'){
      curWP <- homeWP
      if(curWP>0.92){
        base <- WP(wpstates, half.inning, state, count, rdiff + 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base >= 1){
          base <- WP(wpstates, half.inning, state, count, rdiff + 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base >= 1){
          base <- WP(wpstates, half.inning, state, count, rdiff + 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base >= 1){
          base <- WP(wpstates, half.inning, state, count, rdiff + 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        homeWP <- (base/x + curWP)/2 * 100
        print(round(homeWP,3))
        return(homeWP)
      }
      else if(curWP < 0.08){
        base <- WP(wpstates, half.inning, state, count, rdiff + 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff + 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff + 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff + 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        homeWP <- (base/x + curWP)/2 * 100
        print(round(homeWP,3))
        return(homeWP)
      }else{
        return(curWP)
      }
    }
  }else if(rdiff > 0){
    if(substr(half.inning,3,3) == '0'){
      curWP <- awayWP
      if(curWP>0.92){
        base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base >= 1){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base >= 1){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base >= 1){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        awayWP <- (base + change.rdiff(half.inning,state,wpstates)/3 + curWP)/2 * 100
        print(round(awayWP,3))
        return(100 - awayWP)
      }
      else if(curWP < 0.08){
        base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base <= 0.01){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base <= 0.01){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base <= 0.01){
          base <- 1 - WP(wpstates, half.inning, state, count, rdiff - 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        awayWP <- (base/x + curWP)/2 * 100
        print(round(awayWP,3))
        return(100 - awayWP)
      }else{
        return(100 - curWP)
      }
    }else if(substr(half.inning,3,3) == '1'){
      curWP <- homeWP
      if(curWP>0.92){
        base <- WP(wpstates, half.inning, state, count, rdiff - 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base >= 1){
          base <- WP(wpstates, half.inning, state, count, rdiff - 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base >= 1){
          base <- WP(wpstates, half.inning, state, count, rdiff - 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base >= 1){
          base <- WP(wpstates, half.inning, state, count, rdiff - 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        homeWP <- (base/x + curWP)/2 * 100
        print(round(homeWP,3))
        return(homeWP)
      }
      else if(curWP < 0.08){
        base <- WP(wpstates, half.inning, state, count, rdiff - 1, home.team, visiting.team, cs, records, team.info)
        x <- 1
        print(base)
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff - 2, home.team, visiting.team, cs, records, team.info)
          x <- 2
          print(base)}
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff - 3, home.team, visiting.team, cs, records, team.info)
          x <- 3
          print(base)}
        if(base <= 0.01){
          base <- WP(wpstates, half.inning, state, count, rdiff - 4, home.team, visiting.team, cs, records, team.info)
          x <- 4
          print(base)}
        homeWP <- (base/x + curWP)/2 * 100
        print(round(homeWP,3))
        return(homeWP)
      }else{
        return(curWP)
      }
    }
  }else{return(homeWP * 100)}
}
# change NA results from calling WP to zero

state.correction <- function(homeWP, awayWP, count, half.inning, rdiff, state, home.team, visiting.team, cs, records, team.info, wpstates){
  homeWP <- homeWP/100
  awayWP <- awayWP/100
  orderstates <- list('000 2','100 2','000 1','010 2','001 2','110 2','101 2','000 0','100 1','011 2','010 1','111 2','100 0','110 1','001 1','010 0','101 1','011 1','001 0','110 0','111 1','101 0','011 0','111 0')
  if(state != '111 0' & state != '000 2'){
    up.state <- orderstates[match(state, orderstates) + 1]
    baseA <- WP(wpstates, half.inning, as.character(up.state), count, rdiff, home.team, visiting.team, cs, records, team.info)
    down.state <- orderstates[match(state, orderstates) - 1]
    baseB <- WP(wpstates, half.inning, as.character(down.state), count, rdiff, home.team, visiting.team, cs, records, team.info)
    if(base2 < curWP|base3 > curWP){
      correctedWP <- (baseA+baseB)/2
      print(correctedWP)
      x <- error.correction(correctedWP * 100, (1 - correctedWP) * 100, count, half.inning, rdiff, state, home.team, visiting.team, cs, records, team.info, wpstates)
      print(x)
      return(x)}
  }else if(state == '111 0'){
    down.state <- orderstates[match(state, orderstates) - 1]
    lower <- count.state[as.character(down.state), 2]
    actual <- count.state[state, 2]
    d <- actual/lower - 1
    downWP <- homeWP * (1-d)
    correctedWP <- (downWP + homeWP)/2
    print(correctedWP)
    x <- error.correction(correctedWP * 100, (1 - correctedWP) * 100, count, half.inning, rdiff, state, home.team, visiting.team, cs, records, team.info, wpstates)
    print(x)
    return(x)
  }else if(state == '000 2'){
    upWP <- homeWP * 0.9583333
    correctedWP <- (upWP + homeWP)/2
    x <- error.correction(correctedWP * 100, (1 - correctedWP) * 100, count, half.inning, rdiff, state, home.team, visiting.team, cs, records, team.info, wpstates)
    print(x)
    return(x)
  }}

########## Need to comment below functions ###########
change.rdiff <- function(half.inning, state, wpstates = wpstates){
  constants <- paste(half.inning, state)
  x <- subset(wpstates, substr(wpstates$State,1,9) == constants)
  y <- diff(x$WP)
  y <- y[which(y>0)]
  return(mean(y))
}
change.state <- function(half.inning, rdiff, wpstates = wpstates){
  constants <- paste(half.inning, rdiff)
  x <- subset(wpstates, substr(wpstates$State,1,3) == half.inning)
  x <- subset(x, as.numeric(substr(wpstates$State,10, nchar(as.character(wpstates$State)))) == rdiff)
  x$d1 <- (x$WP - x[1,2])
  x$d2 <- (x$WP - x[2,2])
  x$d3 <- (x$WP - x[3,2])
  x$d4 <- (x$WP - x[4,2])
  x$d5 <- (x$WP - x[5,2])
  x$d6 <- (x$WP - x[6,2])
  x$d7 <- (x$WP - x[7,2])
  x$d8 <- (x$WP - x[8,2])
  x$d9 <- (x$WP - x[9,2])
  x$d10 <- (x$WP - x[10,2])
  x$d11 <- (x$WP - x[11,2])
  x$d12 <- (x$WP - x[12,2])
  x$d13 <- (x$WP - x[13,2])
  x$d14 <- (x$WP - x[14,2])
  x$d15 <- (x$WP - x[15,2])
  x$d16 <- (x$WP - x[16,2])
  x$d17 <- (x$WP - x[17,2])
  x$d18 <- (x$WP - x[18,2])
  x$d19 <- (x$WP - x[19,2])
  x$d20 <- (x$WP - x[20,2])
  x$d21 <- (x$WP - x[21,2])
  x$d22 <- (x$WP - x[22,2])
  x$d23 <- (x$WP - x[23,2])
  x$d24 <- (x$WP - x[24,2])
}
rdiff.correction <- function(half.inning, state, rdiff, wpstates){
  perm <- paste(half.inning, state, rdiff, sep = " ")
  if(rdiff > 0){
    permA <-paste(half.inning, state, rdiff - 1, sep = " ")
    permB <-paste(half.inning, state, rdiff - 2, sep = " ")
    permC <-paste(half.inning, state, rdiff - 3, sep = " ")
    permD <-paste(half.inning, state, rdiff - 4, sep = " ")
    permE <-paste(half.inning, state, rdiff - 5, sep = " ")
  }else if(rdiff < 0){
    permA <-paste(half.inning, state, rdiff + 1, sep = " ") 
    permB <-paste(half.inning, state, rdiff + 2, sep = " ")
    permC <-paste(half.inning, state, rdiff + 3, sep = " ")
    permD <-paste(half.inning, state, rdiff + 4, sep = " ")
    permE <-paste(half.inning, state, rdiff + 5, sep = " ")
  }else if(rdiff == 0){
    permA <-paste(half.inning, state, rdiff - 2, sep = " ") 
    permB <-paste(half.inning, state, rdiff - 1, sep = " ") 
    permC <-paste(half.inning, state, rdiff, sep = " ") 
    permD <-paste(half.inning, state, rdiff + 1, sep = " ") 
    permE <-paste(half.inning, state, rdiff + 2, sep = " ") 
  }
  wpstate <- (subset(wpstates, wpstates$State == perm))$WP
  wpstateA <- (subset(wpstates, wpstates$State == permA))$WP
  wpstateB <- (subset(wpstates, wpstates$State == permB))$WP
  wpstateC <- (subset(wpstates, wpstates$State == permC))$WP
  wpstateD <- (subset(wpstates, wpstates$State == permD))$WP
  wpstateE <- (subset(wpstates, wpstates$State == permE))$WP
  print(wpstateA)
  print(wpstateB)
  print(wpstateC)
  print(wpstateD)
  print(wpstateE)
  diffs <- c(abs(wpstateA - wpstateB),abs(wpstateB - wpstateC),abs(wpstateC - wpstateD),abs(wpstateD - wpstateE))
  mean <- mean(diffs, na.rm = TRUE)
  print(mean)
  print(wpstate)
  if(rdiff >0){
    wpstate <- ifelse(wpstateA == 1|is.na(wpstateA == TRUE), ifelse(wpstateB == 1|is.na(wpstateB) == TRUE, min(wpstateC + mean, 99.99), min(wpstateB + mean, 99.99)), min(wpstateA + mean, 99.99))
  }else if(rdiff < 0){
    wpstate <- ifelse(wpstateA == 0|is.na(wpstateA == TRUE), ifelse(wpstateB == 1|is.na(wpstateB) == TRUE, max(wpstateC - mean, 0.01), max(wpstateB - mean, 0.01)), max(wpstateA - mean, 0.01))
  }else if(rdiff == 0){
    wpstateA <- wpstateB + mean
    wpstateB <- wpstateD - mean
    wpstate <- (wpstateA + wpstateB)/2
  }
  print(wpstate)
  wpstate2 <- state.correction(half.inning, state, rdiff, wpstates)
  print(wpstate2)
  
  print((wpstate+wpstate2)/2)
  # return(wpstate)
}
WP <- function(wpstates, half.inning, state, count, rdiff, home.team, visiting.team, cs, records, team.info){
  # current permutation of inning, state, and run differential
  perm <- paste(half.inning, state, rdiff, sep = " ")
  # calculate the run differential if a run was scored in the current half inning
  rdiff2 <- ifelse(substr(half.inning,3,3) == 0, rdiff - 1, rdiff + 1)
  # find the new permutation with the new run differential 
  perm2 <- paste(half.inning, state, rdiff2, sep = " ")
  # find the win probability with the current permutation
  wpstate <- (subset(wpstates, wpstates$State == perm))$WP
  if((subset(wpstates, wpstates$State == perm))$Num < 200){
    print('one')
    wpstate <- corrector(half.inning, state, rdiff, wpstates)
  }
  # find the win probability with the permutation if a run is scored
  wpstate.plus.one.run <- (subset(wpstates, wpstates$State == perm2))$WP
  if((subset(wpstates, wpstates$State == perm2))$Num < 200){
    print('two')
    wpstate.plus.one.run <- corrector(half.inning, state, rdiff, wpstates)
  }
  # calculate the difference in the win probabilities
  state.dif <- (wpstate.plus.one.run - wpstate)
  # find the run expectancy with the current state and count
  re <- cs[state,count]
  # find the general run expectancy just with the base/out state (count back to 0-0) 
  raw.re <- cs[state,2]
  # find the change in the run expectancy
  dif <- (as.numeric(re) - as.numeric(raw.re))/as.numeric(raw.re)
  # multiply the change by the win probability if a run scored
  new.wp <- dif * wpstate.plus.one.run
  delta <- state.dif * dif
  # add the current win probability to the change if a run scored
  dynamic.state <- wpstate + delta
  # adjust if interleague game
  # get league of home team
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
  wp.with.team.standing <- as.numeric(dynamic.state)*(1+x/as.numeric(substr(half.inning,1,1)))
  # correct if the percentage went above 100, so at most, it is 99.99%
  bound <- min(99.99, wp.with.team.standing)
  return(bound)
}
