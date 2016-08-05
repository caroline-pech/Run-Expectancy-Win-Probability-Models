setwd("~/Desktop/run-expectancy")

library(shiny)
library(retrosheet)
library(dplyr)
source("Calculations.R")
library(DT)
library(shinyjs)

shinyServer(function(input, output){
    # if game info button has been pushed, use selected teams to return proper batter and pitcher options
  a <- observe(if(input$info){
      activePitchers <- subset(activePlayers, Position == 'Starter'|Position == 'Reliever'|Position == 'P')
      output$namePitcher <- renderUI({
        pitchers <- (as.character(activePitchers[which(teams[input$pitcher, 2]==activePitchers$Team),1]))
        selectizeInput("namePitcher","Pitcher's Name:", pitchers, options = list(placeholder = 'Please select a pitcher', onInitialize = I('function() { this.setValue(""); }')))})
      output$nameBatter <- renderUI({
        batters <- (as.character(activePlayers[which(teams[input$batter, 2]==activePlayers$Team),1]))
        selectizeInput("nameBatter","Batter's Name:", batters, options = list(placeholder = 'Please select a batter', onInitialize = I('function() { this.setValue(""); }')))})})
  # same as above on different tab
 b <- observeEvent(input$info_button, {
    activePitchers <- subset(activePlayers, Position == 'Starter'|Position == 'Reliever'|Position == 'P')
    batter_team <- ifelse(input$half_inning == 'Top', paste(input$visiting_team), paste(input$home_team))
    pitcher_team <- ifelse(input$half_inning == 'Top', paste(input$home_team), paste(input$visiting_team))
    output$pitcher_name <- renderUI({
      available_pitchers <- (as.character(activePitchers[which(teams[pitcher_team, 2]==activePitchers$Team),1]))
      selectizeInput("pitcher_name","Pitcher's Name:", available_pitchers, options = list(placeholder = 'Please select a pitcher', onInitialize = I('function() { this.setValue(""); }')))})
    output$batter_name <- renderUI({
      available_batters <- (as.character(activePlayers[which(teams[batter_team, 2]==activePlayers$Team),1]))
      selectizeInput("batter_name","Batter's Name:", available_batters, options = list(placeholder = 'Please select a batter', onInitialize = I('function() { this.setValue(""); }')))})})
  # if batter's name has been filled, get win probability button shows up
 c <- observeEvent(input$batter_name,{
    output$state_button <- renderUI({
      actionButton("state_button", "Get Win Probability",style="color: #lightgrey; background-color: #306EFF; border-color: #2e6da4")})})
  # if game info button pushed, display list of batters and pitchers to choose from relative to team inputs (1st comparison player)
  d <- observeEvent(input$midbutton,{
    activePitchers <- subset(activePlayers, Position == 'Starter'|Position == 'Reliever'|Position == 'P')
    batter_team <- ifelse(input$half == 'Top', paste(input$visiting), paste(input$p.home))
    pitcher_team <- ifelse(input$half == 'Bottom', paste(input$visiting), paste(input$p.home))
    output$the_pitcher <- renderUI({
      avail_pitchers <- (as.character(activePitchers[which(teams[pitcher_team, 2]==activePitchers$Team),1]))
      selectizeInput("the_pitcher","Pitcher's Name:", avail_pitchers, options = list(placeholder = 'Please select a pitcher', onInitialize = I('function() { this.setValue(""); }')))})
    output$the_batter <- renderUI({
      avail_batters <- (as.character(activePlayers[which(teams[batter_team, 2]==activePlayers$Team),1]))
      selectizeInput("the_batter","Batter's Name:", avail_batters, options = list(placeholder = 'Please select a batter', onInitialize = I('function() { this.setValue(""); }')))})})
  # if game info button pushed, display list of batters and pitchers to choose from relative to team inputs (2nd comparison player)
  e <- observeEvent(input$midbutton2,{
    activePitchers <- subset(activePlayers, Position == 'Starter'|Position == 'Reliever'|Position == 'P')
    batter_team <- ifelse(input$half2 == 'Top', paste(input$visiting2), paste(input$p.home2))
    pitcher_team <- ifelse(input$half2 == 'Bottom', paste(input$visiting2), paste(input$p.home2))
    output$pitcher2 <- renderUI({
      avail_pitchers <- (as.character(activePitchers[which(teams[pitcher_team, 2]==activePitchers$Team),1]))
      selectizeInput("pitcher2","Pitcher's Name:", avail_pitchers, options = list(placeholder = 'Please select a pitcher', onInitialize = I('function() { this.setValue(""); }')))})
    output$batter2 <- renderUI({
      avail_batters <- (as.character(activePlayers[which(teams[batter_team, 2]==activePlayers$Team),1]))
      selectizeInput("batter2","Batter's Name:", avail_batters, options = list(placeholder = 'Please select a batter', onInitialize = I('function() { this.setValue(""); }')))})})
  # if comparison button has been pushed, have identical column of player info on right hand side of page
  compare <- observeEvent(input$compare_button, {
    shinyjs::hide(input$one_player_comparison)
    output$compare <- renderUI({
      column(width = 6, 
             fluidRow(style = "padding-left:25px;",
                h4("Player 2", style = "color: black; font-style: bold; align:center;"),
                selectizeInput("p.home2","Home Team:", choices = sort(dimnames(teams)[[1]]), options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))),
                tags$head(tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: navy !important;}.selectize-dropdown .active {background: #ADC4EC !important;}"))),
                selectizeInput("visiting2", "Visiting Team", choices = sort(dimnames(teams)[[1]]), options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))),
                radioButtons('half2', 'Inning:', c('Top', 'Bottom'), inline = TRUE),
                actionButton("midbutton2", "Submit Game Info",style="color: #lightgrey; background-color: #306EFF; border-color: #2e6da4;"),
                br(),
                br(),
                uiOutput("pitcher2"),
                uiOutput("batter2"),
                radioButtons('p_outs2','Outs',c('0','1','2'),inline=TRUE),
                br(),
                radioButtons('p_first2','Baserunner on First?',c('No','Yes'),inline=TRUE),
                br(),
                radioButtons('p_second2','Baserunner on Second?',c('No','Yes'),inline=TRUE),
                br(),
                radioButtons('p_third2','Baserunner on Third?',c('No','Yes'),inline=TRUE),
                br(),
                radioButtons('p_balls2','Balls?',c('0','1','2','3'),inline=TRUE),
                br(),
                radioButtons('p_strikes2','Strikes?',c('0','1','2'),inline=TRUE),
                br(),
                actionButton('finishbutton', 'Get Comparison Probabilities', style = "color: #lightgrey; background-color: #306EFF; border-color: #2e6da4;")))})})
  # if all the info has been submitted, run the win probability function from Calculations.R
  WP <- observeEvent(input$state_button,{
    if(input$batter_name == '' || input$pitcher_name == ''){
      output$home <- renderInfoBox({infoBox("Error", paste('Not Valid Players'), icon = icon('fa fa-times'), color = 'red', fill = TRUE)})
    }else{
      if(input$inning > 9){
        input$inning <- 9
      }
      batter_team <- ifelse(input$half_inning == 'Top', input$visiting_team, input$home_team)
      pitcher_team <- ifelse(input$half_inning == 'Top', input$home_team, input$visiting_team)
      runners <- paste(ifelse(input$s_first == 'Yes', 1, 0), ifelse(input$s_second == 'Yes', 1, 0), ifelse(input$s_third == 'Yes', 1, 0), sep="")
      state <- paste(runners, input$s_outs)
      count <- paste('c',input$balls, input$strikes, sep="")
      half <- ifelse(input$half_inning == 'Top', 0, 1)
      half_inning <- paste(input$inning, half, sep = " ")
      b_league <- (as.character(teams[batter_team, 3]))
      p_league <- (as.character(teams[pitcher_team, 3]))
      league <- ifelse(b_league == p_league, b_league, 'inter')
      rdiff <- as.numeric(input$home_score) - as.numeric(input$vis_score)
      if(rdiff > 8){rdiff <- 8}else if(rdiff < -8){rdiff <- -8}
      home_win_prob <- pitcher_batter_WP(wpstates, half_inning, state, count, rdiff, input$home_team, input$visiting_team, input$pitcher_name, input$batter_name, pitcher_team, batter_team, count_state, records, teams, league)
      if(home_win_prob == 'Impossible'){
        output$home <- renderInfoBox({infoBox("Home - Win Prob", paste('Impossible'), icon = icon('fa fa-times'), color = 'red', fill = TRUE)})
        output$away <- renderInfoBox({infoBox("Away - Win Prob", paste('Impossible'), icon = icon('fa fa-times'), color = 'red', fill = TRUE)})
      }else{
        error_check <- min(home_win_prob,0.9999) * 100
        away_win_prob <- round(100 - error_check,3)
        error_check <- round(error_check, 3)
        typea <- ifelse(as.numeric(error_check) > 50, 'fa fa-thumbs-o-up', 'fa fa-thumbs-o-down')
        typeb <- ifelse(as.numeric(away_win_prob) > 50, 'fa fa-thumbs-o-up', 'fa fa-thumbs-o-down')
        colora <- ifelse(as.numeric(error_check) > 50, 'green', 'red')
        colorb <- ifelse(as.numeric(away_win_prob) > 50, 'green', 'red')
        output$home <- renderInfoBox({infoBox("Home - Win Prob", paste(error_check, '%', sep = ""), icon = icon(typea), color = colora, fill = TRUE)})
        output$away <- renderInfoBox({infoBox("Away - Win Prob", paste(away_win_prob, '%', sep = ""), icon = icon(typeb), color = colorb, fill = TRUE)})
      }
    }
  })
  # if the comparison player information has been submitted, run the probabilities function from Calculations.R
  probabilities2 <- observeEvent((input$finishbutton),{
      bases1 <- paste(ifelse(input$p_first == 'Yes', 1, 0), ifelse(input$p_second == 'Yes', 1, 0), ifelse(input$p_third == 'Yes', 1, 0), sep = "")
      bases2 <- paste(ifelse(input$p_first2 == 'Yes', 1, 0), ifelse(input$p_second2 == 'Yes', 1, 0), ifelse(input$p_third2 == 'Yes', 1, 0), sep = "")
      p_state1 <- paste(bases1, input$p_outs)
      p_state2 <- paste(bases2, input$p_outs2)
      p_count1 <- paste('c', input$p_balls, input$p_strikes, sep = "")
      p_count2 <- paste('c', input$p_balls2, input$p_strikes, sep="")
      batter_team1 <- ifelse(input$half == 'Top', input$visiting, input$p.home)
      pitcher_team1 <- ifelse(input$half == 'Top', input$p.home, input$visiting)
      batter_team2 <- ifelse(input$half2 == 'Top', input$visiting2, input$p.home2)
      pitcher_team2 <- ifelse(input$half2 == 'Top', input$p.home2, input$visiting)
      league1 <- ifelse((as.character(teams[batter_team1, 3])) == (as.character(teams[pitcher_team1, 3])), (as.character(teams[batter_team1, 3])), 'inter')
      league2 <- ifelse(as.character(teams[batter_team2, 3]) == as.character(teams[pitcher_team2, 3]), as.character(teams[batter_team2, 3]), 'inter')
      prob1 <- probabilities(batter_team1, pitcher_team1, input$the_batter, input$the_pitcher, league1, p_state1, p_count1)
      prob2 <- probabilities(batter_team2, pitcher_team2, input$batter2, input$pitcher2, league2, p_state2, p_count2)
      x <- deltas(prob1, prob2, input$the_batter, input$the_pitcher, input$batter2, input$pitcher2)
      dimnames(x)[[2]] <- c(paste(input$the_batter, 'v.', input$the_pitcher, sep = " "), paste(input$batter2, 'v.', input$pitcher2, sep = " "), "Higher Probability Matchup", "Difference")
      output$comparisontable = DT::renderDataTable({
        DT::datatable(x, options = list(paging = FALSE, searching = FALSE), rownames = c("Single", "Double", "Triple", "Home Run", "Walk", "Out"))
      })})
  # if all the run expectancy info has been submitted, run fun1 from Calculations.R for return value
  RE <- observe(if(input$state_button2){
    if(input$nameBatter == '' || input$namePitcher == ''){
      output$runs <- renderInfoBox({infoBox("Error", paste('Not Valid Players'), icon = icon('fa fa-times'), color='red', fill = TRUE)})
    }else{
      runners <- paste(ifelse(input$s_first2 == 'Yes', 1, 0), ifelse(input$s_second2 == 'Yes', 1, 0), ifelse(input$s_third2 == 'Yes', 1, 0), sep="")
      state <- paste(runners, input$s_outs2)
      count <- paste('c',input$balls2, input$strikes2, sep="")
      RE <- (fun1(as.character(input$home), as.character(input$batter), as.character(input$pitcher), as.character(input$namePitcher), as.character(state), as.character(count),as.character(input$nameBatter)))
      output$runs <- renderInfoBox({infoBox("Run Expectancy", paste(RE), icon = icon('fa fa-angle-right'), color='navy',fill=TRUE)})
    }
  })})