# Soccer App
# Mars 2018
# ABIDE Nadira - EKAMBI Olivier - GRIMAL Simon
########################################################"""""""


library(shiny)
library(shinydashboard)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Selection", tabName = "front", icon = icon("tasks")),
    menuItem("Analyse du match", icon = icon("bar-chart-o"), tabName = "analyse",
             menuSubItem('Analyse Match',
                         tabName = 'Match',
                         icon = icon('line-chart')),
             menuSubItem('Analyse Joueurs',
                         tabName = 'Joueurs',
                         icon = icon('line-chart'))),
    menuItem("Modélisation", icon = icon("spinner"), tabName = "model"),
    menuItem("Informations", icon = icon("info-circle"), tabName = "about")
  )
)


body <- dashboardBody(
  useShinyjs(),
  tabItems(
    
    ###########################################################################################################################################
    
    tabItem(tabName = "select",
            fluidRow(
              box(
                height = 400, width = 2, title = "Selection du match", solidHeader = TRUE, status = "primary",
                selectInput("dataActionChoice", label = "Sectionnez un championnat", 
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
    
    tabItem(tabName = "analyse"
            ),
    
    
    ###########################################################################################################################################
    
    tabItem(tabName = "modelisation"
            ),
    
    ###########################################################################################################################################
    
    tabItem(tabName = "about",
            h3("Version V0"),
            h3("Selection du match à analyser"),
            h3("Analyse du Match"),
            p("Position des joureurs"),
            p("Statistiques sur les équipes Domicile et extérieur"),
            
            h3("Modélisation"),
            p("Comparaison et résultats des modèles prédictifs"),
            p("Contenu exact à définir précisemment")
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
