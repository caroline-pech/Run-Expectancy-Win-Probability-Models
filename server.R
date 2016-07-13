setwd("~/Desktop/run-expectancy")

library(shiny)
library(retrosheet)
library(dplyr)
source("RE-VA.R")

shinyServer(function(input, output) {
  observeEvent(input$info.button, {output$pitcher.name <- renderUI({
    available.pitchers <- (as.character(pitcherData[which(teams[input$pitcher.team, 5]==pitcherData$mlb_team),4]))
    selectInput("pitcher.name","Pitcher's Name:", available.pitchers)})
  })
  observeEvent(input$info.button, {output$batter.name <- renderUI({
    available.batters <- (as.character(player.info[which(teams[input$batter.team, 5]==player.info$team),1]))
    selectInput("batter.name","Batter's Name:", available.batters)})
  })
  
  RE <- observe(if(input$state.button){
          runners <- paste(ifelse(input$s.first == 'Yes', 1, 0), ifelse(input$s.second == 'Yes', 1, 0), ifelse(input$s.third == 'Yes', 1, 0), sep="")
          state <- paste(runners, input$s.outs)
          count <- paste('c',input$balls, input$strikes, sep="")
          RE <- (fun1(as.character(input$home.team), as.character(input$batter.team), as.character(input$pitcher.team), as.character(input$pitcher.name), as.character(state), as.character(count),as.character(input$batter.name)))
          output$runs <- renderInfoBox({infoBox("Run Expectancy", paste(RE), icon = icon('fa fa-angle-right'), color='navy',fill=TRUE)})
          })
  output$WP <- renderInfoBox({
    b1 <- ifelse(input$first == 'Yes', 1, 0)
    b2 <- ifelse(input$second == 'Yes', 1, 0)
    b3 <- ifelse(input$third == 'Yes', 1, 0)
    runners <- paste(b1,b2,b3, sep = "")
    state <- paste(runners, input$outs, sep = " ")
    rdiff <- as.numeric(input$home.score) - as.numeric(input$vis.score)
    half <- ifelse(input$half.inning == 'Top', 0, 1)
    half.inning <- paste(input$inning, half, sep = " ")
    w.count <- paste('c',input$w.balls, input$w.strikes, sep="")
    WP.raw <- WP(wpstates, half.inning, state, w.count, rdiff, input$h.team, input$v.team, count.state, records)*100
    # (decadedata,half.inning,state,rdiff)*100
    WP <- paste(round(WP.raw, 3), '%', sep="")
    type <- ifelse(as.numeric(WP.raw) > 50, 'fa fa-thumbs-o-up', 'fa fa-thumbs-o-down')
    infoBox("Home ", paste(WP), icon = icon(type), color='red', fill = TRUE)
    })
  output$OP <- renderInfoBox({
    b1 <- ifelse(input$first == 'Yes', 1, 0)
    b2 <- ifelse(input$second == 'Yes', 1, 0)
    b3 <- ifelse(input$third == 'Yes', 1, 0)
    runners <- paste(b1,b2,b3, sep = "")
    state <- paste(runners, input$outs, sep = " ")
    rdiff <- as.numeric(input$home.score) - as.numeric(input$vis.score)
    half <- ifelse(input$half.inning == 'Top', 0, 1)
    half.inning <- paste(input$inning, half, sep = " ")
    w.count <- paste('c',input$w.balls, input$w.strikes, sep="")
    WP <- WP(wpstates, half.inning, state, w.count, rdiff, input$h.team, input$v.team, count.state, records)*100
    OP.raw <- 100 - as.numeric(WP)
    OP <- paste(round(OP.raw, 3), '%', sep="")
    type <- ifelse(as.numeric(OP.raw) > 50, 'fa fa-thumbs-o-up', 'fa fa-thumbs-o-down')
    infoBox("Visiting ", paste(OP), icon =icon(type), color='navy', fill = TRUE)})
    
  })