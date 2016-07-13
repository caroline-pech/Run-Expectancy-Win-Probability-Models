teams <- read.csv("teams2016.csv", header = TRUE)
dimnames(teams)[[1]] <- teams$X
teams$X <- NULL
library(shinydashboard)

sidebar <- dashboardSidebar(sidebarMenu(menuItem("Win Probability", tabName = "wp", icon = icon("fa fa-angle-right")),
                            menuItem("Run Expectancy", tabName = "re", icon = icon("fa fa-angle-right"))))
body <- dashboardBody(style="height:1000px",  
                      
          tabItems(
            tabItem(tabName = "wp", 
                    fluidRow(sidebarPanel(
                      style="font-family:Arial, sans-serif;color:black;",
                      h5(
                      selectInput("h.team","Home Team:", choices = sort(dimnames(teams)[[1]])),
                      selectInput("v.team","Visiting Team:", choices = sort(dimnames(teams)[[1]])),
                      textInput('inning','Inning:',value=1),
                      br(),
                      radioButtons('half.inning','Inning:',c('Top','Bottom'),inline=TRUE),
                      br(),
                      textInput('vis.score','Visitors Score :',value=0),
                      br(),
                      textInput('home.score','Home Score:',value=0),
                      br(),
                      radioButtons('outs','Outs',c('0','1','2'),inline=TRUE),
                      br(),
                      radioButtons('first','Baserunner on First?',c('No','Yes'),inline=TRUE),
                      br(),
                      radioButtons('second','Baserunner on Second?',c('No','Yes'),inline=TRUE),
                      br(),
                      radioButtons('third','Baserunner on Third?',c('No','Yes'),inline=TRUE),
                      br(),
                      radioButtons('w.balls','Balls?',c('0','1','2','3'),inline=TRUE),
                      br(),
                      radioButtons('w.strikes','Strikes?',c('0','1','2'),inline=TRUE))),
                      # sliderInput('success','Base Running Success Rate:', min=0.00, max=1.00,value=c(0.70), step=0.01))),
                      infoBoxOutput("WP"),
                      infoBoxOutput("OP"))),
              tabItem(tabName = "re", fluidRow( 
                      column(width = 8, fluidRow(style="padding-left:25px",
                         selectInput("home.team","Home Team:", choices = sort(dimnames(teams)[[1]])),
                         selectInput("batter.team","Batting Team:", choices = sort(dimnames(teams)[[1]])),
                         selectInput("pitcher.team","Pitcher's Team:", choices = sort(dimnames(teams)[[1]])),
                         actionButton("info.button", "Submit Game Info",style="color: #lightgrey; background-color: #337ab7; border-color: #2e6da4;"),
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
                         actionButton("state.button", "Get Run Expectancy",style="color: #lightgrey; background-color: #337ab7; border-color: #2e6da4")
                         )),
                      infoBoxOutput('runs')))))
dashboardPage(
  dashboardHeader(title = "RE & WP"),
  sidebar,
  body,
  skin = 'black'
)
