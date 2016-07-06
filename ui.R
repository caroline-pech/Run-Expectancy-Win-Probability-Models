library(shiny)
library(shinydashboard)
library(retrosheet)
setwd("/srv/shiny-server/Run-Expectancy")
year <- '2016'
teams <- data.frame(getTeamIDs(as.numeric(year)-1))
teams$batter.team <- (c('laa','bal','bos','chw','cle','det','hou','kc','min','nyy','oak','sea','tb', 'tex','tor','ari','atl','chc','cin','col','la','mia','mil','nym','phi','pit','sd','sf','stl','was'))
teams$league <- (c('AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','AL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL','NL'))
teams$full.name <- (c('Los Angeles Angels of Anaheim','Baltimore Orioles','Boston Red Sox','Chicago White Sox','Cleveland Indians','Detriot Tigers','Houston Astros','Kansas City Royals','Minnesota Twins','New York Yankees','Oakland Athletics','Seattle Mariners','Tampa Bay Rays','Texas Rangers','Toronto Blue Jays','Arizona Diamondbacks','Atlanta Braves','Chicago Cubs','Cincinnati Reds','Colorado Rockies','Los Angeles Dodgers','Miami Marlins','Milwaukee Brewers','New York Mets','Philadelphia Phillies','Pittsburgh Pirates','San Diego Padres','San Francisco Giants','St. Louis Cardinals','Washington Nationals'))
teams$pitching.data.team <- (c('LAA','BAL','BOS','CWS','CLE','DET','HOU','KC','MIN','NYY','OAK','SEA','TB','TEX','TOR','ARI','ATL','CHC','CIN','COL','LAD','MIA','MIL','NYM','PHI','PIT','SD','SF','STL','WSH'))
teams$field <- (c('Angel Stadium of Anaheim','Oriole Park at Camden Yards','Fenway Park II','U.S. Cellular Field','Progressive Field','Comerica Park','Minute Maid Park','Kauffman Stadium','Target Field','Yankee Stadium III','O.co Coliseum','Safeco Field','Tropicana Field','Rangers Ballpark in Arlington','Rogers Centre','Chase Field','Turner Field','Wrigley Field','Great American Ball Park','Coors Field','Dodger Stadium','Marlins Park','Miller Park','Citi Field','Citizens Bank Park','PNC Park','Petco Park','AT&T Park','Busch Stadium III','Nationals Park'))
teams$built.year <- (c(2012,1992,1934,2003,2012,2000,2002,1993,2010,2009,2012,1999,1998,2007,2005,2006,1997,1914,2003,1995,1962,2012,2001,2009,2004,2001,2004,2006,2006,2008))
dimnames(teams)[[2]][1] <- ('home.team')
pitcherData <- read.csv('pitcherData.csv', header = TRUE)
pitcherData <- subset(pitcherData, pitcherData$mlb_pos == 'P')
pitcherData <- pitcherData[c(4,14,24,25,26)]
pitcherData <- pitcherData[-which(pitcherData$retro_id == ""), ]
dimnames(pitcherData)[[1]] = paste(pitcherData$mlb_team, pitcherData$retro_name)

dashboardPage(dashboardHeader(title ="Run Expectancy Generator"), 
              dashboardSidebar(sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("list")))),
              skin = "red", 
              dashboardBody(
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidRow(
                            selectInput("home.team","Home Team:", choices = dimnames(teams)[[1]]),
                            selectInput("batter.team","Batting Team:", choices = dimnames(teams)[[1]]),
                            selectInput("pitcher.team","Pitcher's Team:", choices = dimnames(teams)[[1]]),
                            actionButton("info.button", "Submit Game Info"),
                            selectInput("state","State:", choices = c("000 0","100 0","010 0","001 0","110 0","101 0","011 0","111 0","000 1","100 1","010 1","001 1","110 1","101 1","011 1","111 1","000 2","100 2","010 2","001 2","110 2","101 2","011 2","111 2")),
                            # selectInput("base.runners","Baserunners:", choices = c("000","100","010","001","110","101","011","111")),
                            # selectInput("outs","Outs:", choices = c(0, 1, 2, 3)),
                            selectInput("count","Count:", choices = c('c00', 'c10', 'c20', 'c30', 'c01', 'c02', 'c11', 'c21', 'c31', 'c12', 'c22', 'c32')),
                            uiOutput("pitcher.name"),
                            uiOutput("batter.name"),
                            actionButton("state.button", "Get Run Expectancy"),
                            div(strong(h3("Run Expectancy")), h4(textOutput("runs")),style="text-align:center;")
                            )
                          )
                        )
                      )
                  )
