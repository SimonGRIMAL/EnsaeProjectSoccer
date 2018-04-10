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
    menuItem("Selection", tabName = "Select", icon = icon("tasks"),
             selectInput(inputId = "ChoixChampionnat", label = "Selectionnez un championnat", 
                         choices = League$name,
                         selected = 1),
             selectInput(inputId = "ChoixMatch", label = "Selectionnez un match", 
                          choices = uiOutput(outputId = "MatchSelection"))),
    menuItem("Analyse du match", icon = icon("bar-chart-o"), tabName = "analyse",
             menuSubItem('Analyse Match',
                         tabName = 'Match',
                         icon = icon('line-chart')),
             menuSubItem('Analyse Joueurs',
                         tabName = 'Joueurs',
                         icon = icon('line-chart'))),
    menuItem("ModC)lisation", icon = icon("spinner"), tabName = "model"),
    menuItem("Informations", icon = icon("info-circle"), tabName = "about")
  )
)


body <- dashboardBody(
  #useShinyjs(),
  tabItems(
    
    ###########################################################################################################################################
    
    tabItem(tabName = "Select",
            fluidRow(
              box(
                height = 400, width = 2, title = "Selection du match", solidHeader = TRUE, status = "primary",
                selectInput("ChoixChampionnat", label = "Sectionnez un championnat", 
                            choices = list(""), 
                            selected = 1),
                selectInput("dataActionChoice", label = "Sectionnez une equipe domicile", 
                            choices = list(""), 
                            selected = 1),
                selectInput("dataActionChoice", label = "Sectionnez une equipe extérieur", 
                            choices = list(""), 
                            selected = 1)
              )
                )
    ),
    ###########################################################################################################################################
    
    tabItem(tabName = "Match",
            plotOutput(outputId="goalPlot"),
            dataTableOutput(outputId="teamData")
            ),
    
    
    ###########################################################################################################################################
    
    tabItem(tabName = "modelisation"
            ),
    
    ###########################################################################################################################################
    
    tabItem(tabName = "about",
            h3("Version V0"),
            h3("Selection du match C  analyser"),
            h3("Analyse du Match"),
            p("Position des joureurs"),
            p("Statistiques sur les C)quipes Domicile et extC)rieur"),
            
            h3("ModC)lisation"),
            p("Comparaison et rC)sultats des modC(les prC)dictifs"),
            p("Contenu exact C  dC)finir prC)cisemment")
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
