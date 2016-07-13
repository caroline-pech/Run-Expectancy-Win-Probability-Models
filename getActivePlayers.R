library("devtools")
devtools::install_github("stattleship/stattleship-r")
library(stattleshipR)
set_token("bf3c65fd3952ea434f4a96b641744475")
league <- "mlb"
sport <- "baseball"
ep <- "players"
q_body <- list()
players <- ss_get_result(sport = sport, league = league, ep = ep,
                         query = q_body, version = 1, walk = TRUE)
players_df <- do.call("rbind", lapply(players, function(x) x$players))

batters <- read.csv("batters.csv")