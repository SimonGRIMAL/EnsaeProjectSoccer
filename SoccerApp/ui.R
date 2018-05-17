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
    menuItem("Selection", tabName = "selection", icon = icon("tasks")),
    
    menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis",
             menuSubItem('Configuration',
                         tabName = 'match',
                         icon = icon('line-chart')),
             menuSubItem('Team',
                         tabName = 'team',
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
            box(width=2,
              selectInput(inputId = "ChoixChampionnat", label = "Championship Choice", 
                        choices = League$name,
                       selected = "France Ligue 1"),
            uiOutput(outputId = "HomeTeamSelection"),    #selectInput dependant du precedent selectInput -> dans Server.R
            uiOutput(outputId = "AwayTeamSelection")   #selectInput dependant du precedent selectInput -> dans Server.R
            ),
            box(width=10,
                title="pas moyen de mettre une variable dependante ici même dans le titre sans quoi les selections des équipes disparaissent!!!!"
                )
            ),
    
    
    ###########################################################################################################################################
    
    tabItem(tabName = "match",
            box(title = "System of Play",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                tableOutput(outputId="playerHomeData"),
                tableOutput(outputId="playerAwayData")
                
              ),

              imageOutput("terrainVisu")
            
            ),
    
    tabItem(tabName = "team",
       fluidRow(
            box(title="Home Team : Goals scored and conceded",
                status="primary", 
                solidHeader = TRUE,
                collapsible = TRUE,
                textOutput(outputId="NameHTeam"),
                tags$head(tags$style("#NameHTeam{color: green;
                                     font-size: 17px;
                                     font-style: italic;
                                     font-weight: bold;
                                     }")
                         ),
                plotOutput(outputId="goalHPlot")
            ),
            
            box(title="Away team : goals scored and conceded",
                status="danger", 
                solidHeader = TRUE,
                collapsible = TRUE,
                textOutput(outputId="NameATeam"),
                tags$head(tags$style("#NameATeam{color: green;
                                     font-size: 17px;
                                     font-style: italic;
                                     font-weight: bold;
                                     }")
                ),
                plotOutput(outputId="goalAPlot")
            )
      ) ,
      fluidRow(
        box(title="Home team attributes",
            status="primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
           # h3("Attributes of the team",
           #    style="color: green;
           #    font-size: 20px;
           #    font-style: italic;
           #    font-weight: bold;"),
            tableOutput(outputId="teamHData")
            ),
        
        box(title="Away team attributes",
            status="danger", 
            solidHeader = TRUE,
            collapsible = TRUE,
           # h3("Attributes of the team",
           #    style="color: green;
           #    font-size: 20px;
           #    font-style: italic;
           #    font-weight: bold;"),
           tableOutput(outputId="teamAData")
            )
            ) 
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
