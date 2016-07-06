library(dplyr)

shinyServer(function(input, output) {
  batters <- read.csv("batters.csv", header = TRUE)
  batters <- batters[c(2,3,4)]
  library(shiny)
  source("RE-VA.R")
  observeEvent(input$info.button, {output$pitcher.name <- renderUI({
    available.pitchers <- (as.character(pitcherData[which(teams[input$pitcher.team, 5]==pitcherData$mlb_team),4]))
    selectInput("pitcher.name","Pitcher's Name:", available.pitchers)
  })})
  observeEvent(input$info.button, {output$batter.name <- renderUI({
    available.batters <- (as.character(batters[which(teams[input$batter.team, 5]==batters$mlb_team),1]))
    selectInput("batter.name","Batter's Name:", available.batters)})
  })
  RE <- observe(if(input$state.button){
                     RE <- (fun1(as.character(input$home.team), as.character(input$batter.team), as.character(input$pitcher.team), as.character(input$pitcher.name), as.character(input$state), as.character(input$count),as.character(input$batter.name)))
                     output$runs <- renderText(RE)})
})
