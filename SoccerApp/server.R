library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)


#Téléchargement des objets R nécessaires
load("data/League.Rdata")
load("data/Team.Rdata")
load("data/Team_home_viz.RData")
load("data/Team_away_viz.RData")
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
  
  #Deduction Selection du match #### Match_id ####  A VOIR SI CA MARCHE 
  Match_id <- reactive({
    Match_Shiny %>% filter(home_team_api_name==input$ChoixEquipeMaison | away_team_api_name==input$ChoixEquipeExterieur) %>% select(match_api_id)
  })
  Match_id=3000 #for debug
  
  #tabItem = team (box de gauche Position Joueurs)
  ################################################
  library(magick)
  image <- image_read("img/terrain2.jpg")

  output$terrainVisu <- renderImage({
    width<-413
    height<-825
    size <- paste(width,'x',height)
    X_home<-width/10*t(Match_Shiny[Match_id,12:22])  #calcul coord team home
    Y_home<-height/2/12*t(Match_Shiny[Match_id,34:44])
    
    X_away <- width/10*t(Match_Shiny[Match_id,23:33])  #calcul coord team away
    Y_away <- height/2/12*t(Match_Shiny[Match_id,45:55])+2*(height/2-height/2/12*t(Match_Shiny[Match_id,45:55]))
    
    tmpfile <- image %>%
      image_resize(size) %>%
      #image_draw(image, pointsize = 20,antialias = FALSE) %>%
      #points(X_home,Y_home,pch=6,col="blue",bg="blue") %>%
      #points(X_away,Y_away,pch=2,col="red",bg="red") %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
  
  
  #tabItem = team (box de droite stats Equipe)
  ############################################
  
  output$NameHTeam <- renderText({
                          paste("Home team : ",input$ChoixEquipeMaison)
                                })
  
  output$goalHPlot <- renderPlot({
    
      #fonction filter_team dans helpers.R
      home_team_g<-filter_team(Team_home_viz,input$ChoixEquipeMaison)
      
      home_team_g$season <- substr(as.character(home_team_g$variable),nchar(as.character(home_team_g$variable))-8,nchar(as.character(home_team_g$variable)) )
      home_team_g$variable <- substr(as.character(home_team_g$variable),1,nchar(as.character(home_team_g$variable))-10 )
      
      ggplot(home_team_g)+ aes(x =season, y=value, group=variable, colour=variable) +
      geom_point(size=2) +
      geom_line(size=1) + 
      theme_classic() +
      geom_text(aes(label=value), nudge_y = 1, size=3 ) +
      labs(title="Number of goals scored and conceded at home by season", x= "Season", y ="Number of goals") +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("red", "green"))
    
  })
  
  output$teamHData <- renderTable({
                 
                home_team_g <- filter(Team_home_viz,team_long_name==input$ChoixEquipeMaison)
                t_htg<-as.data.frame(t(home_team_g [,51:62]))
                t_htg$v<-rownames(t_htg)
                colnames(t_htg) <- c(""," ")
                t_htg<-t_htg[,c(2,1)]
                return(t_htg)
  })
  
  output$NameATeam <- renderText({
    paste("Home team : ",input$ChoixEquipeExterieur)
  })
  
  output$goalAPlot <- renderPlot({
    
    #fonction filter_team dans helpers.R
    away_team_g<-filter_team(Team_away_viz,input$ChoixEquipeExterieur)
    
    away_team_g$season <- substr(as.character(away_team_g$variable),nchar(as.character(away_team_g$variable))-8,nchar(as.character(away_team_g$variable)) )
    away_team_g$variable <- substr(as.character(away_team_g$variable),1,nchar(as.character(away_team_g$variable))-10 )
    
    ggplot(away_team_g)+ aes(x =season, y=value, group=variable, colour=variable) +
      geom_point(size=2) +
      geom_line(size=1) + 
      theme_classic() +
      geom_text(aes(label=value), nudge_y = 1, size=3 ) +
      labs(title="Number of goals scored and conceded away by season", x= "Season", y ="Number of goals") +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("red", "green"))
    
  })
  
  output$teamAData <- renderTable({
    
    away_team_g <- filter(Team_away_viz,team_long_name==input$ChoixEquipeExterieur)
    t_atg<-as.data.frame(t(away_team_g [,51:62]))
    t_atg$v<-rownames(t_atg)
    colnames(t_atg) <- c(""," ")
    t_atg<-t_atg[,c(2,1)]
    return(t_atg)
  })
  
  
})