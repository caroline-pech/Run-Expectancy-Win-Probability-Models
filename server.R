setwd("~/Desktop/run-expectancy")

library(shiny)
library(retrosheet)
library(dplyr)
source("RE-VA.R")

shinyServer(function(input, output) {
  a <- observe(if(input$info){
      output$namePitcher <- renderUI({
        available.pitchers <- (as.character(pitcherData[which(teams[input$pitcher, 5]==pitcherData$mlb_team),4]))
        selectizeInput("namePitcher","Pitcher's Name:", available.pitchers, options = list(placeholder = 'Please select a pitcher', onInitialize = I('function() { this.setValue(""); }')))})
      output$nameBatter <- renderUI({
        available.batters <- (as.character(activePlayers[which(teams[input$batter, 2]==activePlayers$Team),1]))
        selectizeInput("nameBatter","Batter's Name:", available.batters, options = list(placeholder = 'Please select a batter', onInitialize = I('function() { this.setValue(""); }')))})})
  b <- observeEvent(input$info.button, {
    batter.team <- ifelse(input$half.inning == 'Top', paste(input$visiting.team), paste(input$home.team))
    pitcher.team <- ifelse(input$half.inning == 'Top', paste(input$home.team), paste(input$visiting.team))
    output$pitcher.name <- renderUI({
      available.pitchers <- (as.character(pitcherData[which(teams[pitcher.team, 5]==pitcherData$mlb_team),4]))
      selectizeInput("pitcher.name","Pitcher's Name:", available.pitchers, options = list(placeholder = 'Please select a pitcher', onInitialize = I('function() { this.setValue(""); }')))})
    output$batter.name <- renderUI({
      available.batters <- as.character(activePlayers[which(teams[batter.team, 2]==activePlayers$Team),1])
      selectizeInput("batter.name","Batter's Name:", available.batters, options = list(placeholder = 'Please select a batter', onInitialize = I('function() { this.setValue(""); }')))})})
  c <- observeEvent(input$batter.name,{
    output$state.button <- renderUI({
      actionButton("state.button", "Get Win Probability",style="color: #lightgrey; background-color: #337ab7; border-color: #2e6da4")
    })
  })
  WP <- observeEvent(input$state.button,{
    if(input$batter.name == '' || input$pitcher.name == ''){
      output$home <- renderInfoBox({infoBox("Error", paste('Not Valid Players'), icon = icon('fa fa-times'), color = 'red', fill = TRUE)})
    }else{
      if(input$inning > 9){
        input$inning <- 9
      }
      batter.team <- ifelse(input$half.inning == 'Top', input$visiting.team, input$home.team)
      pitcher.team <- ifelse(input$half.inning == 'Top', input$home.team, input$visiting.team)
      runners <- paste(ifelse(input$s.first == 'Yes', 1, 0), ifelse(input$s.second == 'Yes', 1, 0), ifelse(input$s.third == 'Yes', 1, 0), sep="")
      state <- paste(runners, input$s.outs)
      count <- paste('c',input$balls, input$strikes, sep="")
      half <- ifelse(input$half.inning == 'Top', 0, 1)
      half.inning <- paste(input$inning, half, sep = " ")
      b.league <- (as.character(teams[batter.team, 3]))
      p.league <- (as.character(teams[pitcher.team, 3]))
      league <- ifelse(b.league == p.league, b.league, 'inter')
      rdiff <- as.numeric(input$home.score) - as.numeric(input$vis.score)
      if(rdiff > 8){rdiff <- 8}else if(rdiff < -8){rdiff <- -8}
      home.win.prob <- pitcher.batter.WP(wpstates, half.inning, state, count, rdiff, input$home.team, input$visiting.team, input$pitcher.name, input$batter.name, pitcher.team, batter.team, count.state, records, teams, league)
      print(home.win.prob)
      if(home.win.prob == 'Impossible'){
        output$home <- renderInfoBox({infoBox("Home - Win Prob", paste('Impossible'), icon = icon('fa fa-times'), color = 'red', fill = TRUE)})
        output$away <- renderInfoBox({infoBox("Away - Win Prob", paste('Impossible'), icon = icon('fa fa-times'), color = 'red', fill = TRUE)})
      }else{
        error.check <- min(home.win.prob,0.9999) * 100
        away.win.prob <- round(100 - error.check,3)
        error.check <- round(error.check, 3)
        typea <- ifelse(as.numeric(error.check) > 50, 'fa fa-thumbs-o-up', 'fa fa-thumbs-o-down')
        typeb <- ifelse(as.numeric(away.win.prob) > 50, 'fa fa-thumbs-o-up', 'fa fa-thumbs-o-down')
        colora <- ifelse(as.numeric(error.check) > 50, 'green', 'red')
        colorb <- ifelse(as.numeric(away.win.prob) > 50, 'green', 'red')
        output$home <- renderInfoBox({infoBox("Home - Win Prob", paste(error.check, '%', sep = ""), icon = icon(typea), color = colora, fill = TRUE)})
        output$away <- renderInfoBox({infoBox("Away - Win Prob", paste(away.win.prob, '%', sep = ""), icon = icon(typeb), color = colorb, fill = TRUE)})
      }
    }
  })
  RE <- observe(if(input$state.button2){
    if(input$nameBatter == '' || input$namePitcher == ''){
      output$runs <- renderInfoBox({infoBox("Error", paste('Not Valid Players'), icon = icon('fa fa-angle-right'), color='navy',fill=TRUE)})
    }else{
      runners <- paste(ifelse(input$s.first2 == 'Yes', 1, 0), ifelse(input$s.second2 == 'Yes', 1, 0), ifelse(input$s.third2 == 'Yes', 1, 0), sep="")
      state <- paste(runners, input$s.outs2)
      count <- paste('c',input$balls2, input$strikes2, sep="")
      RE <- (fun1(as.character(input$home), as.character(input$batter), as.character(input$pitcher), as.character(input$namePitcher), as.character(state), as.character(count),as.character(input$nameBatter)))
      output$runs <- renderInfoBox({infoBox("Run Expectancy", paste(RE), icon = icon('fa fa-angle-right'), color='navy',fill=TRUE)})
    }
  })}
)