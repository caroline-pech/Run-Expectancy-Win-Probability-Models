teams <- read.csv("teams2016.csv", header = TRUE)
dimnames(teams)[[1]] <- teams$X
teams$X <- NULL
library(shinydashboard)

sidebar <- dashboardSidebar(width = 250, sidebarMenu(
                            menuItem("Win Probability - Matchups", tabName = 'wpSpecific', icon = icon('fa fa-angle-right')),
                            menuItem("Run Expectancy", tabName = "re", icon = icon("fa fa-angle-right"))))
body <- dashboardBody(  
          tabItems(
              tabItem(tabName = "re", style="height:1000px", fluidRow(
                      column(width = 8, fluidRow(style="padding-left:25px",
                         selectizeInput("home","Home Team:", choices = sort(dimnames(teams)[[1]]),options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput("batter","Batting Team:", choices = sort(dimnames(teams)[[1]]), options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))), 
                         tags$head(
                           tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: navy !important;}
                      .selectize-dropdown .active {background: #ADC4EC !important;}"))),
                         selectizeInput("pitcher","Pitcher's Team:", choices = sort(dimnames(teams)[[1]]), options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))),
                         actionButton("info", "Submit Game Info",style="background-color: #337ab7; border-color: #2e6da4;"),
                         br(),
                         br(),
                         uiOutput("namePitcher"),
                         br(),
                         uiOutput("nameBatter"),
                         br(),
                         radioButtons('s.outs2','Outs',c('0','1','2'),inline=TRUE),
                         br(),
                         radioButtons('s.first2','Baserunner on First?',c('No','Yes'),inline=TRUE),
                         br(),
                         radioButtons('s.second2','Baserunner on Second?',c('No','Yes'),inline=TRUE),
                         br(),
                         radioButtons('s.third2','Baserunner on Third?',c('No','Yes'),inline=TRUE),
                         br(),
                         radioButtons('balls2','Balls?',c('0','1','2','3'),inline=TRUE),
                         br(),
                         radioButtons('strikes2','Strikes?',c('0','1','2'),inline=TRUE),
                         actionButton("state.button2", "Get Run Expectancy",style="color: #lightgrey; background-color: #337ab7; border-color: #2e6da4"))),
                      infoBoxOutput("runs"))),
            tabItem(tabName = 'wpSpecific',style="height:1290px",
                      column(width = 8, fluidRow(style="padding-left:25px",
                          selectizeInput("home.team","Home Team:", choices = sort(dimnames(teams)[[1]]), options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))),
                          tags$head(
                            tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: navy !important;}
                                            .selectize-dropdown .active {background: #ADC4EC !important;}"))),
                          selectizeInput("visiting.team", "Visiting Team", choices = sort(dimnames(teams)[[1]]), options = list(placeholder = 'Please select a team', onInitialize = I('function() { this.setValue(""); }'))),
                          textInput('inning','Inning:',value=1),
                          radioButtons('half.inning','Inning:',c('Top','Bottom'),inline=TRUE),
                          br(),
                          actionButton("info.button", "Submit Game Info",style="color: #lightgrey; background-color: #337ab7; border-color: #2e6da4;"),
                          br(),
                          br(),
                          br(),
                          textInput('vis.score','Visitors Score :',value=0),
                          br(),
                          textInput('home.score','Home Score:',value=0),
                          br(),
                          uiOutput("pitcher.name"),
                          br(),
                          uiOutput("batter.name"),
                          br(),
                          radioButtons('s.outs','Outs',c('0','1','2'),inline=TRUE),
                          br(),
                          radioButtons('s.first','Baserunner on First?',c('No','Yes'),inline=TRUE),
                          br(),
                          radioButtons('s.second','Baserunner on Second?',c('No','Yes'),inline=TRUE),
                          br(),
                          radioButtons('s.third','Baserunner on Third?',c('No','Yes'),inline=TRUE),
                          br(),
                          radioButtons('balls','Balls?',c('0','1','2','3'),inline=TRUE),
                          br(),
                          radioButtons('strikes','Strikes?',c('0','1','2'),inline=TRUE),
                          actionButton("state.button", "Get Win Probability",style="color: #lightgrey; background-color: #337ab7; border-color: #2e6da4"))),
                          infoBoxOutput("home"),
                          infoBoxOutput("away"))))
dashboardPage(
  dashboardHeader(title = span(tagList(icon("fa fa-shield"), "numberFire RE & WP")), titleWidth = 250),
  sidebar,
  body,
  skin = 'black'
)
