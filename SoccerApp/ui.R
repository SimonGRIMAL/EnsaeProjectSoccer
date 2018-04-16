# Soccer App
# Mars 2018
# ABIDE Nadira - EKAMBI Olivier - GRIMAL Simon
########################################################

#################
#INITIALISTAION
#################
library(shinyjs)
library(shinydashboard)
library(plotly)
library(shinycssloaders)

load("data/League.Rdata")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Selection", tabName = "selection", icon = icon("tasks"),
             selectInput(inputId = "ChoixChampionnat", label = "Championship Choice", 
                         choices = League$name,
                         selected = 1),
             uiOutput(outputId = "HomeTeamSelection"),    #selectInput dependant du precedent selectInput -> dans Server.R
             uiOutput(outputId = "AwayTeamSelection")),   #selectInput dependant du precedent selectInput -> dans Server.R
    
    menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis",
             menuSubItem('Team',
                         tabName = 'team',
                         icon = icon('line-chart')),
             menuSubItem('Players',
                         tabName = 'players',
                         icon = icon('line-chart'))),
    menuItem("Modelisation", icon = icon("spinner"), tabName = "modelisation"),
    menuItem("Informations", icon = icon("info-circle"), tabName = "about")
  )
)


body <- dashboardBody(
  #useShinyjs(),
  tabItems(
    
    ###########################################################################################################################################
    
    tabItem(tabName = "selection",
            img(src = "img/logo_ligue1.jpeg")
    ),
    ###########################################################################################################################################
    
    tabItem(tabName = "team",
            box(width = 3,
              imageOutput("terrainVisu")
            ),
            box(title="Statistics about home and away team",
                status="success", 
                solidHeader = TRUE,
                collapsible = TRUE,
                textOutput(outputId="NameHTeam"),
                plotOutput(outputId="goalHPlot"),
                tableOutput(outputId="teamHData"),
                textOutput(outputId="NameATeam"),
                plotOutput(outputId="goalAPlot"),
                tableOutput(outputId="teamAData")
                )
            ),
    
    tabItem(tabname = "players",
            h2("ici stats sur les joueurs")
            ),
    
    ###########################################################################################################################################
    
    tabItem(tabName = "modelisation",
            h2("ici la modelisation")
    ),
    ###########################################################################################################################################
    
    tabItem(tabName = "about",
            h3("Version V0"),
            h3("Select Match Season 2015/2016"),
            h3("Match Analyse"),
            p("Players position"),
            p("Team statistics"),
            
            h3("Modelisation"),
            p("Predictives models based on season 2008 to 2015"),
            p("And more")
    )
  )
)

shinyUI(
  dashboardPage(skin="green",
                dashboardHeader(title = "Soccer Data App"),
                dashboardSidebar(sidebar),
                body
  )
)
