setwd("~/Desktop/RE&WP/Final.RE")
library(retrosheet)
year <- 2016
teams <- data.frame(getTeamIDs(current.year - 1))
teams$batter.team <- (c('laa','bal','bos','chw','cle','det','hou','kc','min','nyy','oak','sea','tb', 'tex','tor','ari','atl','chc','cin','col','la','mia','mil','nym','phi','pit','sd','sf','stl','was'))
teams$pitching.data.team <- (c('LAA','BAL','BOS','CWS','CLE','DET','HOU','KC','MIN','NYY','OAK','SEA','TB','TEX','TOR','ARI','ATL','CHC','CIN','COL','LAD','MIA','MIL','NYM','PHI','PIT','SD','SF','STL','WSH'))
dimnames(teams)[[2]][1] <- ('home.team')
pitcherData <- read.csv('pitcherData.csv', header = TRUE)
pitcherData <- subset(pitcherData, pitcherData$mlb_pos == 'P')
pitcherData <- pitcherData[c(4,14,24,25,26)]
pitcherData <- pitcherData[-which(pitcherData$retro_id == ""), ]
batters <- read.csv("batters.csv", header = TRUE)
batters <- batters[c(2,3,4)]
# library(shiny)

shinyServer(function(input, output) {
  source("RE-VariableAnalysis2.R")
  observeEvent(input$info.button, {output$pitcher.name <- renderUI({
    available.pitchers <- (as.character(pitcherData[which(teams[input$pitcher.team, 3]==pitcherData$mlb_team),4]))
    selectInput("pitcher.name","Pitcher's Name:", available.pitchers)
  })})
  observeEvent(input$info.button, {output$batter.name <- renderUI({
    available.batters <- (as.character(batters[which(teams[input$batter.team, 3]==batters$mlb_team),1]))
    selectInput("batter.name","Batter's Name:", available.batters)})
  })
  RE <- observe(if(input$state.button){
                     RE <- (fun1(as.character(input$home.team), as.character(input$batter.team), as.character(input$pitcher.team), as.character(input$pitcher.name), as.character(input$state), as.character(input$count),as.character(input$batter.name)))
                     output$runs <- renderText(RE)})
})
