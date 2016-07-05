setwd("~/Desktop/RE&WP")
data <- read.csv("gamedata_1990-2015.csv", header = FALSE)
headers <- read.csv("fields.csv")
names(data) <- headers$Header
#################################################
get.run.env <- function(data){
  # get the base/out states
  data$RUNS <- with(data, AWAY_SCORE_CT + HOME_SCORE_CT)
  data$HALF.INNING <- with(data, paste(GAME_ID, INN_CT, BAT_HOME_ID))
  
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  
  RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, list(HALF.INNING = data$HALF.INNING), sum)
  
  RUNS.SCORED.START <- aggregate(data$RUNS, list(HALF.INNING = data$HALF.INNING), "[", 1)
  
  MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  data <- merge(data, MAX)
  N <- ncol(data)
  names(data)[N] <- "MAX.RUNS"
  
  data$RUNS.ROI <- c(data$MAX.RUNS - data$RUNS)
  
  #get 24-state matrix
  get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }
  
  RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID==1|BAT_DEST_ID==1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID==2|RUN2_DEST_ID==2|BAT_DEST_ID==2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID==3|RUN2_DEST_ID==3|RUN3_DEST_ID==3|BAT_DEST_ID==3))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  data$NEXT.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  
  data <- subset(data, (STATE!=NEXT.STATE)|(RUNS.SCORED>0))
  
  library(plyr)
  data.outs <- ddply(data, .(HALF.INNING), summarize, Outs.Inning = sum(EVENT_OUTS_CT))
  data <- merge(data, data.outs)
  dataC <- subset(data, Outs.Inning == 3)
  
  RUNS <- with(dataC, aggregate(RUNS.ROI, list(STATE), mean))
  RUNS$Outs <- substr(RUNS$Group, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]
  
  RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
  dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")
  
  RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3","010 3","011 3","100 3","101 3","110 3","111 3")
  data$RUNS.STATE <- RUNS.POTENTIAL[data$STATE, ]
  data$RUNS.NEXT.STATE <- RUNS.POTENTIAL[data$NEXT.STATE, ]
  data$RUNS.VALUE <- data$RUNS.NEXT.STATE - data$RUNS.STATE + data$RUNS.SCORED
  
  dataC <- subset(data, BAT_EVENT_FL == TRUE)
  
  library(car)
  dataC$NEXT.STATE <- recode(dataC$NEXT.STATE,"c('000 3', '100 3', '010 3', '001 3','110 3', '101 3', '011 3', 
                            '111 3') = '3'")
  
  # Computing the transition probabilities
  
  TR.MATRIX <- with(dataC, table(STATE, NEXT.STATE))
  
  PR.MATRIX <- prop.table(TR.MATRIX, 1)
  
  PR.MATRIX <- rbind(PR.MATRIX, c(rep(0, 24), 1))
  
  count.runners.outs <- function(s){
    sum(as.numeric(strsplit(s,"")[[1]]), na.rm = TRUE)
  }
  runners.outs <- sapply(dimnames(TR.MATRIX)[[1]], count.runners.outs)[-25]
  R <- outer(runners.outs + 1, runners.outs, FUN="-")
  dimnames(R)[[1]] <- dimnames(TR.MATRIX)[[1]][-25]
  dimnames(R)[[2]] <- dimnames(TR.MATRIX)[[1]][-25]
  R <- cbind(R, rep(0, 24))
  
  # simulate chain
  
  simulate.half.inning <- function(P, R, start=1){
    s <- start; path <- NULL; runs <- 0
    while(s < 25){
      s.new <- sample(1:25, 1, prob = P[s, ])
      path <- c(path, s.new)
      runs <- runs + R[s, s.new]
      s <- s.new
    }
    runs
  }
  
  RUNS <- replicate(10000, simulate.half.inning(TR.MATRIX, R))
  
  
  RUNS.i <- function(i){
    mean(replicate(10000, simulate.half.inning(TR.MATRIX, R, i)))
  }
  
  Runs.Expectancy <- sapply(1:24, RUNS.i)
  Runs.Expectancy <- t(round(matrix(Runs.Expectancy, 3, 8), 3))
  Runs.Expectancy <- cbind(Runs.Expectancy, 0)
  dimnames(Runs.Expectancy)[[2]] <- (outs = c("0", "1", "2", "3"))
  dimnames(Runs.Expectancy)[[1]] <- (runners = c("000", "001", "010", "011", "100", "101", "110", "111"))
  
  #Count Analysis
  data$pitch.seq <- gsub("[.>123N+*]", "", data$PITCH_SEQ_TX)
  
  data$c00 <- TRUE
  data$c10 <- grepl("^[BIPV]", data$pitch.seq)
  data$c01 <- grepl("^[CFKLMOQRST]", data$pitch.seq)
  data$c20 <- grepl("^[BIPV]{2}", data$pitch.seq)
  data$c30 <- grepl("^[BIPV]{3}", data$pitch.seq)
  data$c02 <- grepl("^[CFKLMOQRST]{2}", data$pitch.seq)
  data$c11 <- grepl("^([CFKLMOQRST][BIPV]|[BIPV][CFKLMOQRST])", data$pitch.seq)
  data$c21 <- grepl("^([CFKLMOQRST][BIPV][BIPV]|[BIPV][CFKLMOQRST][BIPV]|[BIPV][BIPV][CFKLMOQRST])", data$pitch.seq)
  data$c31 <- grepl("^([CFKLMOQRST][BIPV][BIPV][BIPV]|[BIPV][CFKLMOQRST][BIPV][BIPV]|[BIPV][BIPV][CFKLMOQRST]
                    [BIPV]|[BIPV][BIPV][BIPV][CFKLMOQRST])", data$pitch.seq)
  data$c12 <- grepl("^([CFKLMOQRST][CFKLMOQRST][FR]*[BIPV]|[BIPV][CFKLMOQRST][CFKLMOQRST]|[CFKLMOQRST][BIPV]
                    [CFKLMOQRST])", data$pitch.seq)
  data$c22 <- grepl("^([CFKLMOQRST][CFKLMOQRST][FR]*[BIPV][FR]*[BIPV]
                    |[BIPV][BIPV][CFKLMOQRST][CFKLMOQRST]
                    |[BIPV][CFKLMOQRST][BIPV][CFKLMOQRST]
                    |[BIPV][CFKLMOQRST][CFKLMOQRST][FR]*[BIPV]
                    |[CFKLMOQRST][BIPV][CFKLMOQRST][FR]*[BIPV]
                    |[CFKLMOQRST][BIPV][BIPV][CFKLMOQRST])", data$pitch.seq)
  data$c32 <- grepl("^([CFKLMOQRST][CFKLMOQRST][BIPV][BIPV][BIPV]|[CFKLMOQRST][CFKLMOQRST][FR]*[BIPV][BIPV][BIPV]|
                    [CFKLMOQRST][CFKLMOQRST][BIPV][FR]*[BIPV][BIPV]|[CFKLMOQRST][CFKLMOQRST][BIPV][BIPV][FR]*[BIPV]|
                    [CFKLMOQRST][BIPV][BIPV][BIPV][CFKLMOQRST]|[CFKLMOQRST][BIPV][CFKLMOQRST][BIPV][BIPV]|
                    [CFKLMOQRST][BIPV][CFKLMOQRST][FR]*[BIPV][BIPV]|[CFKLMOQRST][BIPV][BIPV][CFKLMOQRST][FR]*[BIPV]|
                    [CFKLMOQRST][BIPV][BIPV][CFKLMOQRST][BIPV]|[CFKLMOQRST][BIPV][CFKLMOQRST][BIPV][FR]*[BIPV]|
                    [BIPV][BIPV][BIPV][CFKLMOQRST][CFKLMOQRST]|[BIPV][CFKLMOQRST][CFKLMOQRST][FR]*[BIPV][BIPV]|
                    [BIPV][CFKLMOQRST][CFKLMOQRST][BIPV][BIPV]|[BIPV][CFKLMOQRST][BIPV][BIPV][CFKLMOQRST]|
                    [BIPV][CFKLMOQRST][BIPV][CFKLMOQRST][BIPV]|[BIPV][CFKLMOQRST][BIPV][CFKLMOQRST][FR]*[BIPV]|
                    [BIPV][BIPV][CFKLMOQRST][CFKLMOQRST][BIPV]|[BIPV][BIPV][CFKLMOQRST][CFKLMOQRST][FR]*[BIPV]|
                    [BIPV][BIPV][CFKLMOQRST][BIPV][CFKLMOQRST])", data$pitch.seq)
  
  runs.by.count <- expand.grid(balls=0 : 3, strikes = 0 : 2)
  runs.by.count$value <- 0
  
  bs.count.run.value <- function(b, s){
    column.name <- paste("c", b, s, sep="")
    mean(data[data[, column.name] == 1, "RUNS.VALUE"])
  }
  
  runs.by.count$value <- mapply(FUN=bs.count.run.value,b=runs.by.count$balls, s=runs.by.count$strikes)
  
  #combine balls/strikes columns into a count column
  runs.by.count[,1] <- paste(runs.by.count[,1], runs.by.count[,2], sep="-")
  runs.by.count$strikes <- NULL
  runs.by.count <- rename(runs.by.count, c("balls" = "count"))
  
  # Combine 24 state values with count values
  runs.by.count.runners.outs <- data.frame(as.vector(Runs.Expectancy))
  dimnames(runs.by.count.runners.outs)[[1]] <- (states = c("000 0", "001 0", "010 0", "011 0","100 0","101 0",
                                                           "110 0","111 0","000 1", "001 1", "010 1", "011 1","100 1","101 1","110 1","111 1","000 2", "001 2", "010 2", 
                                                           "011 2","100 2","101 2","110 2","111 2","000 3", "001 3", "010 3", "011 3","100 3","101 3","110 3","111 3"))
  dimnames(runs.by.count.runners.outs)[[2]] <- (values = c("RUNNERS.OUTS.STATES"))
  
  runs.by.count.runners.outs$c00 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[1,2]),3)
  runs.by.count.runners.outs$c10 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[2,2]),3)
  runs.by.count.runners.outs$c20 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[3,2]),3)
  runs.by.count.runners.outs$c30 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[4,2]),3)
  runs.by.count.runners.outs$c01 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[5,2]),3)
  runs.by.count.runners.outs$c11 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[6,2]),3)
  runs.by.count.runners.outs$c21 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[7,2]),3)
  runs.by.count.runners.outs$c31 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[8,2]),3)
  runs.by.count.runners.outs$c02 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[9,2]),3)
  runs.by.count.runners.outs$c12 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[10,2]),3)
  runs.by.count.runners.outs$c22 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[11,2]),3)
  runs.by.count.runners.outs$c32 <- round(c(runs.by.count.runners.outs$RUNNERS.OUTS.STATES + runs.by.count[12,2]),3)
  # Exclude absorbing state where outs == 3
  Runs.Expectancy.With.Count <- subset(runs.by.count.runners.outs, RUNNERS.OUTS.STATES != 0)
  return(Runs.Expectancy.With.Count)
}

matrix <- get.run.env(data)
write.csv(Runs.Expectancy.With.Count, file = paste("~/Desktop/RE&WP", '1990.2015.RE.States.Count.csv'), row.names = TRUE)