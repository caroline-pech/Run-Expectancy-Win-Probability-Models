library(RCurl)
# decadedata is a file with all the play-by-play files from 2010 - last complete season (2015)
# the winners column is not necessary -- just need a merged file of all the years you want data from
decadedata <- read.csv("decadedata-withWinners.csv", header = TRUE)
# site used to get ids : http://crunchtimebaseball.com/baseball_map.html
# read the csv 
html <- getURL("http://crunchtimebaseball.com/master.csv")
id_table <- read.csv(text = html)
# select the necessary columns and only pitchers
pitcher_table <- subset(id_table, mlb_pos == "P")
pitcher_table <- pitcher_table[,c(4,14,24,25,26)]
# get wOBA values from guts table
guts_table <- read.csv("guts_table_2016.csv", header = TRUE)
dimnames(guts_table)[[1]] <- guts_table$X
guts_table$X <- NULL
# this function gets a pitchers splits and average wOBA. 
get_pitcher_splits <- function(pitcher.retrosheet.id, table=guts_table, year = '2016', data = decadedata){
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
    return(c(avg,pitcher.L.wOBA,avg,pitcher.retrosheet.id))
  }  
  if (LH.PA < 500 & RH.PA > 500){
    return(c(pitcher.R.wOBA, avg, avg,pitcher.retrosheet.id))
  }
  if (RH.PA >= 500 & LH.PA >= 500){
    return(c(pitcher.R.wOBA,pitcher.L.wOBA, avg,pitcher.retrosheet.id))
  }
  if (RH.PA < 500 & LH.PA < 500){
    return(c(avg,avg,avg,pitcher.retrosheet.id))
  }
}
# apply every row of pitcher_table to get all pitchers' splits
x <- sapply(as.character(pitcher_table$retro_id), get_pitcher_splits)
# transpose the matrix to a vertical matrix
y <- t(x)
# set NaN values to zero
y[is.nan(y)] = 0
# delete rows with all zero values (empty ids)
y <- y[ rowSums(y)!=0, ]
# create a data frame from the matrix and keep the matrix rownames (the retrosheet ids) in a new column
b <- data.frame(y, row.name = rownames(y))
# name the columns
dimnames(b)[[2]] <- c("Right", "Left", "Average", "retro_id")
# merge the two data tables by the unique retro ids
z <- merge(b, pitcher_table, by = 'retro_id')
# delete duplicated rows
z<- z[!duplicated(z), ]
# give row names
dimnames(z)[[1]] <- paste(z$mlb_team, z$retro_name)
# write to csv
write.csv(z, 'pitcher.info.csv')