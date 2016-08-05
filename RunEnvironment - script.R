# update year as necessary
year <- '2016'
# load python file to scrape Run Environment (runs scored/game)
suppressWarnings(python.load('Run Environment - scrape.py'))
current.environment <- python.call("get_run_environment")
Run.Env <- read.csv("Run.Environments.csv", header = TRUE)
# if year already in table, update the value. Otherwise, insert a row with the new year and value
if(year %in% dimnames(Run.Env)[[1]]){
  Run.Env[year,] <- current.environment
}else{
  Run.Env <- rbind(Run.Env, current.environment)
  i <- nrow(Run.Env)
  dimnames(Run.Env)[[1]][i] <- year
}
# write to csv
write.csv(Run.Env, 'Run.Environments.csv')