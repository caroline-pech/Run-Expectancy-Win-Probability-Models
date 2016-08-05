setwd("~/Desktop/RE&WP")
# gamedata_1990-2015.csv is compiled csv with each year of retrosheet files
data <- read.csv("gamedata_1990-2015.csv", header = FALSE) 
headers <- read.csv("fields.csv")
names(data) <- headers$Header

get.run.env <- function(data){
  # runs sums the scores of the two teams at each PA
  data$RUNS <- with(data, AWAY_SCORE_CT + HOME_SCORE_CT)
  data$HALF.INNING <- with(data, paste(GAME_ID, INN_CT, BAT_HOME_ID))
  # runs scored for each play
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  # runs scored in the half inning
  RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, list(HALF.INNING = data$HALF.INNING), sum)
  # runs scored since the beginning of the half inning
  RUNS.SCORED.START <- aggregate(data$RUNS, list(HALF.INNING = data$HALF.INNING), "[", 1)
  # calculate maximum total score in a half-inning  (initial total runs + runs scored)
  MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  # x = maximum runs scored
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  # add the max runs to the data file and name it "MAX.RUNS"
  data <- merge(data, MAX)
  N <- ncol(data)
  names(data)[N] <- "MAX.RUNS"
  # ROI = total runs scored in inning - current runs scored
  data$RUNS.ROI <- c(data$MAX.RUNS - data$RUNS)
  
  #get state (baserunners and outs)
  get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }
  # if there is a baserunner on, assign it a "1", otherwise a "0" for getting the current state
  RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  # same as above but with destination states -- this is the new state
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID==1|BAT_DEST_ID==1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID==2|RUN2_DEST_ID==2|BAT_DEST_ID==2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID==3|RUN2_DEST_ID==3|RUN3_DEST_ID==3|BAT_DEST_ID==3))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  data$NEXT.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  # subset data where the event causes the state to change or run(s) is/are scored
  data <- subset(data, (STATE!=NEXT.STATE)|(RUNS.SCORED>0))
  library(plyr)
  # only use half.innings where a full three outs are recorded -- called dataC
  data.outs <- ddply(data, .(HALF.INNING), summarize, Outs.Inning = sum(EVENT_OUTS_CT))
  data <- merge(data, data.outs)
  dataC <- subset(data, Outs.Inning == 3)
  # expected number of runs scored in the remainder of the inning (run expectancy for 24 states)
  RUNS <- with(dataC, aggregate(RUNS.ROI, list(STATE), mean))
  # run values
  RUNS$Outs <- substr(RUNS$Group, 5, 5)
  # order RUNS data frame by the number of outs
  RUNS <- RUNS[order(RUNS$Outs), ]
  # create matrix with outs on horizontal and states on vertical 
  RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
  dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")
  # run expectancies for the 32 situations including 3 out states -- run expectancy is zero in those situations so set value = 0
  RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3","010 3","011 3","100 3","101 3","110 3","111 3")
  # run expectancy of the current state
  data$RUNS.STATE <- RUNS.POTENTIAL[data$STATE, ]
  # run expectancy of the new state
  data$RUNS.NEXT.STATE <- RUNS.POTENTIAL[data$NEXT.STATE, ]
  # total run value = (next state value - current state value) + any runs that scored
  data$RUNS.VALUE <- data$RUNS.NEXT.STATE - data$RUNS.STATE + data$RUNS.SCORED
  # subset data where their is a batter event
  dataC <- subset(data, BAT_EVENT_FL == TRUE)
  # create the 25th state for the encompassing 3rd out case
  library(car)
  dataC$NEXT.STATE <- recode(dataC$NEXT.STATE,"c('000 3', '100 3', '010 3', '001 3','110 3', '101 3', '011 3', 
                            '111 3') = '3'")
  
  # computing the transition probabilities
  # frequencies of all possible transitions between states. 24 beginning states and 25 possible new states (to account for 3 out state)
  TR.MATRIX <- with(dataC, table(STATE, NEXT.STATE))
  # transition matrix converted to probability matrix using prop.table function
  PR.MATRIX <- prop.table(TR.MATRIX, 1)
  # add row for 3 out state and set probability of staying in 3 out states = 1
  PR.MATRIX <- rbind(PR.MATRIX, c(rep(0, 24), 1))
  # return sum of runners and outs for a state as input 
  count.runners.outs <- function(s){
    sum(as.numeric(strsplit(s,"")[[1]]), na.rm = TRUE)
  }
  # apply all states to the count.runners.outs
  runners.outs <- sapply(dimnames(TR.MATRIX)[[1]], count.runners.outs)[-25]
  # perform RUNS calculation for all pairs of states (current state to new state)
  R <- outer(runners.outs + 1, runners.outs, FUN="-")
  dimnames(R)[[1]] <- dimnames(TR.MATRIX)[[1]][-25]
  dimnames(R)[[2]] <- dimnames(TR.MATRIX)[[1]][-25]
  # add a last column of zeros for three out state
  R <- cbind(R, rep(0, 24))
  
  # simulate chain. P = probability matrix, R = run matrix, s = starting state (24 possibilities). Returns number of runs scored in half inning
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
  RUNS.i <- function(i){
    mean(replicate(10000, simulate.half.inning(TR.MATRIX, R, i)))
  }
  # run simulate half.inning 10000 times with TR.Matrix and R with each possible starting state and find mean which equals run expectancy
  Runs.Expectancy <- sapply(1:24, RUNS.i)
  # make a 3x8 matrix from Runs.Expectancy (each value rounded to the third decimal place) and then transpose
  Runs.Expectancy <- t(round(matrix(Runs.Expectancy, 3, 8), 3))
  # again, set column of zeros for three out state
  Runs.Expectancy <- cbind(Runs.Expectancy, 0)
  # make the matrix with outs on horizontal and runner states on vertical
  dimnames(Runs.Expectancy)[[2]] <- (outs = c("0", "1", "2", "3"))
  dimnames(Runs.Expectancy)[[1]] <- (runners = c("000", "001", "010", "011", "100", "101", "110", "111"))
  #Count Analysis 
  # remove unnecessary symbols
  data$pitch.seq <- gsub("[.>123N+*]", "", data$PITCH_SEQ_TX)
  # all counts start at 0-0 -- therefore true for all batters
  data$c00 <- TRUE
  # if the pitch sequence contains these symbols - the batter had the count at some point during the at-bat
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
  # create a grid for all counts and initially set to zero
  runs.by.count <- expand.grid(balls=0 : 3, strikes = 0 : 2)
  runs.by.count$value <- 0
  # function to count the run value for each count using data file
  bs.count.run.value <- function(b, s){
    column.name <- paste("c", b, s, sep="")
    mean(data[data[, column.name] == 1, "RUNS.VALUE"])
  }
  # apply all possible counts to above function
  runs.by.count$value <- mapply(FUN=bs.count.run.value,b=runs.by.count$balls, s=runs.by.count$strikes)
  #combine balls/strikes columns into a count column
  runs.by.count[,1] <- paste(runs.by.count[,1], runs.by.count[,2], sep="-")
  runs.by.count$strikes <- NULL
  runs.by.count <- rename(runs.by.count, c("balls" = "count"))
  
  # Combine 24 state values with count values
  runs.by.count.runners.outs <- data.frame(as.vector(Runs.Expectancy))
  # vertical is states (including runners and outs)
  dimnames(runs.by.count.runners.outs)[[1]] <- (states = c("000 0", "001 0", "010 0", "011 0","100 0","101 0",
                                                           "110 0","111 0","000 1", "001 1", "010 1", "011 1","100 1","101 1","110 1","111 1","000 2", "001 2", "010 2", 
                                                           "011 2","100 2","101 2","110 2","111 2","000 3", "001 3", "010 3", "011 3","100 3","101 3","110 3","111 3"))
  # horizontal is values for all 24 states
  dimnames(runs.by.count.runners.outs)[[2]] <- (values = c("RUNNERS.OUTS.STATES"))
  # for each count value add the corresponding run value and round to the third decimal place
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
# run the function with data (updated year to year) and write run expectancy matrix to csv to use with RE-VA.R file
matrix <- get.run.env(data)
write.csv(matrix,'1990.2015.RE.States.Count.csv', row.names = TRUE)