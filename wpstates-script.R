# Need to get new data at the beginning of each season for both play-by-play file and gamelogs file 
# Merge new season with current data (so 15 years of data in 2016 -- 16 years of data in 2017, etc) -- terminal function: cat newfile(i.e. GL2016.csv) allfiles(i.e. gamelogs.merged.csv) > gamelogs.merged.csv
# run script with updated data
# for play-by-play file: http://www.retrosheet.org/game.html (i.e. decadedata.csv)
# for gamelogs file: http://www.retrosheet.org/gamelogs/index.html (i.e. gamelogs.merged.csv)

# function to get win probability given a certain permutation
getWP <- function(perm){
  half.inning <- substr(perm,1,3)
  state <- substr(perm,5,9)
  numChar <- nchar(perm)
  rdiff <- substr(perm,11,numChar)
  d <- subset(x, x$STATE == as.character(state) & x$RDIFF == as.numeric(rdiff) & x$HALF.INNING == as.character(half.inning))
  pos <- sum(d$winner == as.character(d$HOME_TEAM))
  total <- nrow(d)
  prob <- pos/total
  print(prob)
  return(c(prob, round(total,0)))
}
# read large data file - will take around 10 minutes
data <- read.csv("decadedata.csv", header = FALSE)
headers <- read.csv("fields.csv")
names(data) <- headers$Header
# calculate half inning, run differential, and state
data$HALF.INNING <- with(data, paste(GAME_ID, INN_CT, BAT_HOME_ID))
data$RDIFF <- data$HOME_SCORE_CT - data$AWAY_SCORE_CT
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
}
RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
# get rid of unnecessary columns
data <- data[,c(1,2,3,9,10,98,99,100)]
# read gamelogs file 
gamelogs <- read.csv("gamelogs.merged.csv", header = FALSE)
gamelogs <- gamelogs[ ,c(1:12)]
headers <- read.csv("fields2.csv")
names(gamelogs) <- headers$Header
# append winner of each game, game id and merge data and game logs files
gamelogs$winner <- ifelse(gamelogs$HOME_SCORE > gamelogs$VISITING_SCORE, paste(gamelogs$HOME_TEAM), paste(gamelogs$VISITING_TEAM))
gamelogs$GAME_ID <- paste(gamelogs$HOME_TEAM, gamelogs$DATE, gamelogs$GAME_NUMBER, sep = "")
x <- merge(data, gamelogs, by = 'GAME_ID')
# include only inning number and top or bottom delegation for half inning
x$HALF.INNING <- substr(x$HALF.INNING, 14,16)
# get all permutations for run differential (-10 to 10), inning, and state
rdiff <- c('-10', '-9', '-8', '-7', '-6', '-5', '-4', '-3', '-2', '-1', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10')
half <- c('1 0', '2 0', '3 0', '4 0', '5 0', '6 0', '7 0', '8 0', '9 0','1 1', '2 1', '3 1', '4 1', '5 1', '6 1', '7 1', '8 1', '9 1')
state <- c("000 0", "000 1", "000 2", "100 0", "100 1", "100 2", "010 0", "010 1", "010 2", "001 0", "001 1", "001 2", "110 0","110 1","110 2", "011 0", "011 1", "011 2", "101 0", "101 1", "101 2", "111 0", "111 1", "111 2")
perms <- expand.grid(rdiff,half,state)
dimnames(perms)[[2]] <- c("rdiff","half.inning","state")
perms$combo <- paste(perms$half.inning, perms$state, perms$rdiff)
# apply all the permuatations to the above function (will take 90 minutes or so to run)
win.probs.for.perms <- sapply(perms$combo, getWP)
# transpose the matrix and write to csv
transposed <- t(win.probs.for.perms)

write.csv(transposed, 'wpstates.csv')