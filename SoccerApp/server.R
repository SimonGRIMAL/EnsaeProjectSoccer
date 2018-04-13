library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)


#Téléchargement des objets R nécessaires
load("data/League.Rdata")
load("data/Team.Rdata")
load("data/Team_home_viz.RData")
load("data/Match_Shiny.Rdata")

#Chargement des sources
source("helpers.R")


shinyServer( function(input, output) {
   
  #tabItem = Selection
  ####################
  
  #Selection du championnat
  #renvoyé dans input$ChoixChampionnat
  
  #Selection de la team Home
  output$HomeTeamSelection <- renderUI({
    Championnat_id <- League[League$name == input$ChoixChampionnat,"id"]
    Equipe_id <- Match_Shiny %>% filter(league_id==Championnat_id) %>% select(home_team_api_id)
    Equipe_id_home <- as.numeric(levels(factor(Equipe_id$home_team_api_id)))
    Choice <- Team %>% filter(team_api_id %in% Equipe_id_home) %>% select(team_long_name)
    selectInput(inputId = "ChoixEquipeMaison", label = "Home Team Selection", 
                choices =  Choice)# ou en une seule ligne Match_Shiny %>% filter(league_id==League[League$name == input$ChoixChampionnat,"id"]) %>% select(home_team_api_name))
  })
  #Selection de la team Away
   output$AwayTeamSelection <- renderUI({
     Championnat_id <- League[League$name == input$ChoixChampionnat,"id"]
     Equipe_id <- Match_Shiny %>% filter(league_id==Championnat_id) %>% select(away_team_api_id)
     Equipe_id_away <- as.numeric(levels(factor(Equipe_id$away_team_api_id)))
     Choice2 <- Team %>% filter(team_api_id %in% Equipe_id_away) %>% select(team_long_name)
     selectInput(inputId = "ChoixEquipeExterieur", label = "Away Team Selection", 
                 choices =  Choice2)
   })
  
  #Deduction Selection du match
  Match_id <- reactive({
    Match_Shiny %>% filter(home_team_api_name==input$ChoixEquipeMaison | away_team_api_name==input$ChoixEquipeExterieur) %>% select(match_api_id)
  })
  
  
  #tabItem == Match
  ##################
  output$goalPlot <- renderPlot({
      #ce n'est pas dynamique
      # exemple avec Paris
    
    paris <- filter( Team_home_viz,team_long_name.x=="Paris Saint-Germain")
    
    paris4<-melt(paris[,c("nb_goal_scored_2008/2009","nb_goal_scored_2009/2010",
                          "nb_goal_scored_2010/2011","nb_goal_scored_2011/2012",
                          "nb_goal_scored_2012/2013","nb_goal_scored_2013/2014",
                          "nb_goal_scored_2014/2015",
                          "nb_goal_conceded_2008/2009","nb_goal_conceded_2009/2010",
                          "nb_goal_conceded_2010/2011","nb_goal_conceded_2011/2012",
                          "nb_goal_conceded_2012/2013","nb_goal_conceded_2013/2014",
                          "nb_goal_conceded_2014/2015"
    )])
    
    paris4$season <- substr(as.character(paris4$variable),nchar(as.character(paris4$variable))-8,nchar(as.character(paris4$variable)) )
    
    paris4$variable <- substr(as.character(paris4$variable),1,nchar(as.character(paris4$variable))-10 )
    
      
      ggplot(paris4)+ aes(x =season, y=value, group=variable, colour=variable) +
      geom_point(size=2) +
      geom_line(size=1) + 
      theme_classic() +
      geom_text(aes(label=value), nudge_y = 4, size=3 ) +
      labs(title="Nombre de buts marqués et encaissés par saison", x= "Saison", y ="Nombre de buts") +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("red", "green"))
    
  })
  
  output$teamData <- renderDataTable({as.data.frame(t(paris [,51:62]))})
  
})