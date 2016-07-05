library(shiny)
library(shinydashboard)

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