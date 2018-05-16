library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)


#Téléchargement des objets R nécessaires
load("data/League.Rdata")
load("data/Team.Rdata")
load("data/Team_home_viz.RData")
load("data/Team_away_viz.RData")
load("data/Match_Shiny.RData")
load("data/player.RData")
load("data/Player_viz.RData")

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
                choices =  Choice, selected = "Paris Saint-Germain")# ou en une seule ligne Match_Shiny %>% filter(league_id==League[League$name == input$ChoixChampionnat,"id"]) %>% select(home_team_api_name))
  })
  #Selection de la team Away
   output$AwayTeamSelection <- renderUI({
     Championnat_id <- League[League$name == input$ChoixChampionnat,"id"]
     Equipe_id <- Match_Shiny %>% filter(league_id==Championnat_id) %>% select(away_team_api_id)
     Equipe_id_away <- as.numeric(levels(factor(Equipe_id$away_team_api_id)))
     Choice2 <- Team %>% filter(team_api_id %in% Equipe_id_away) %>% select(team_long_name)
     selectInput(inputId = "ChoixEquipeExterieur", label = "Away Team Selection", 
                 choices =  Choice2,selected = "FC Nantes")
   })
  
  #Deduction Selection du match #### Match_id ####  A VOIR SI CA MARCHE 
  Match_id=3000 #for debug
  #Match_id <- reactive({
  #  Match_Shiny %>% filter(home_team_api_name==input$ChoixEquipeMaison | away_team_api_name==input$ChoixEquipeExterieur) %>% select(match_api_id)
  #})
  
  
  #tabItem = team (box de gauche Position Joueurs)
  ################################################
  library(magick)
  image <- image_read("img/terrain2.jpg")

  output$terrainVisu <- renderImage({
    
    Match <- filter_match(Match_Shiny,input$ChoixEquipeMaison,input$ChoixEquipeExterieur)

    #modif position gaol keeper
    Match$home_player_X1 <- Match$home_player_X1+4
    Match$away_player_X1 <- Match$away_player_X1+4
    
    echelle <- 1
    width<-396 * echelle
    height<-768 *echelle
    size <- paste(width,'x',height)
    X_home<-(width/10)*t(Match[,12:22])  #calcul coord team home
    Y_home<-(height/2/12)*t(Match[,34:44])
    
    X_away <- (width/10)*t(Match[,23:33])  #calcul coord team away
    Y_away <- (height/2/12)*t(Match[,45:55])+2*(height/2-height/2/12*t(Match[,45:55]))
    
    ID_home <- t(Match[,56:66])
    ID_away <- t(Match[,67:77])
    Players_home <- filter(Player,Player$player_api_id %in% c(ID_home[1],ID_home[2],ID_home[3],ID_home[4],ID_home[5],ID_home[6],
                                                              ID_home[7],ID_home[8],ID_home[9],ID_home[10],ID_home[11]))
    Players_away <- filter(Player,Player$player_api_id %in% c(ID_away[1],ID_away[2],ID_away[3],ID_away[4],ID_away[5],ID_away[6],
                                                              ID_away[7],ID_away[8],ID_away[9],ID_away[10],ID_away[11]))
    
    #pour affichage Noms sur terrain
    library(tidyr)
    Players_home_details<-separate(Players_home,player_name , into = c("first_name", "last_name"))
    Players_away_details<-separate(Players_away,player_name , into = c("first_name", "last_name"))
    
    position <- image_draw(image, pointsize = 20,antialias = FALSE)
    points(X_home,Y_home,pch=25,col="white",bg="blue",cex=2)
    points(X_away,Y_away,pch=24,col="white",bg="red",cex=2)
    text(X_home,Y_home,col="white",cex=.75,pos=3,labels=Players_home_details$last_name,font=2)
    text(X_away,Y_away,col="white",cex=.75,pos=1,labels=Players_away_details$last_name,font=2)
    
    tmpfile <- position %>%
      image_resize(size) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
  
  #Affichage des tableux de joueurs

  
  output$playerHomeData <- renderTable({
    Match <- filter_match(Match_Shiny,input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    ID_home <- t(Match[,56:66])
    Players_home <- filter(Player,Player$player_api_id %in% c(ID_home[1],ID_home[2],ID_home[3],ID_home[4],ID_home[5],ID_home[6],
                                                              ID_home[7],ID_home[8],ID_home[9],ID_home[10],ID_home[11]))
    
    Players_home$id <- NULL
    Players_home$player_api_id <- NULL
    Players_home$player_fifa_api_id <- NULL
    Players_home
  })
  

  output$playerAwayData <- renderTable({
    Match <- filter_match(Match_Shiny,input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    ID_away <- t(Match[,67:77])
    Players_away <- filter(Player,Player$player_api_id %in% c(ID_away[1],ID_away[2],ID_away[3],ID_away[4],ID_away[5],ID_away[6],
                                                              ID_away[7],ID_away[8],ID_away[9],ID_away[10],ID_away[11]))
    Players_away$id <- NULL
    Players_away$player_api_id <- NULL
    Players_away$player_fifa_api_id <- NULL
    Players_away
  })
  
  output$championnatImage <- renderImage({
    filename <- normalizePath(file.path('./img',
                                        paste(input$ChoixChampionnat,'.jpg', sep='')))
    # filename <- "./img/France Ligue 1.jpeg"
    list(src = filename,
         alt = paste("image",input$ChoixChampionnat)
    )
    
  }, deleteFile = FALSE)
  
  
  #tabItem = team (box de droite stats Equipe)
  ############################################
  
  output$NameHTeam <- renderText({
                      paste("Number of goals scored and conceced by ",input$ChoixEquipeMaison, "when playing at home")
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
      labs(x= "Season", y ="Number of goals") +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("red", "green"))
    
  })
  
  output$teamHData <- renderTable({
                 
                home_team_g <- filter(Team_home_viz,team_long_name==input$ChoixEquipeMaison)
                home<-extract_attributes_team (home_team_g,"home")
                
                away_team_g <- filter(Team_away_viz,team_long_name==input$ChoixEquipeExterieur)
                away <- extract_attributes_team (away_team_g,"away")
                
                mix<- mef_attributes_team(home,away)
              
                return(mix[,c(1,2)])
                
  }, colnames=FALSE, sanitize.text.function = function(x) x, striped=TRUE)
  
  output$NameATeam <- renderText({
                      paste("Number of goals scored and conceced by ",input$ChoixEquipeExterieur, "when playing away")
  })
  
  output$goalAPlot <- renderPlot({
    
    away_team_g<-filter_team(Team_away_viz,input$ChoixEquipeExterieur)
    
    away_team_g$season <- substr(as.character(away_team_g$variable),nchar(as.character(away_team_g$variable))-8,nchar(as.character(away_team_g$variable)) )
    away_team_g$variable <- substr(as.character(away_team_g$variable),1,nchar(as.character(away_team_g$variable))-10 )
    
    ggplot(away_team_g)+ aes(x =season, y=value, group=variable, colour=variable) +
      geom_point(size=2) +
      geom_line(size=1) + 
      theme_classic() +
      geom_text(aes(label=value), nudge_y = 1, size=3 ) +
      labs(x= "Season", y ="Number of goals") +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("red", "green"))
    
  })
  
  output$teamAData <- renderTable({
    
    home_team_g <- filter(Team_home_viz,team_long_name==input$ChoixEquipeMaison)
    home<-extract_attributes_team (home_team_g,"home")
    
    away_team_g <- filter(Team_away_viz,team_long_name==input$ChoixEquipeExterieur)
    away <- extract_attributes_team (away_team_g,"away")
    
    mix<- mef_attributes_team(home,away)
    return(mix[,c(1,3)])

  }, colnames=FALSE, sanitize.text.function = function(x) x, striped=TRUE)
  
  
  #tabItem = players
  ############################################

  output$PlayerH1Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),1])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH2Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),2])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH3Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),3])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH4Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),4])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH5Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),5])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH6Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),6])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH7Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),7])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH8Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),8])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH9Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),9])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH10Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),10])
  }, colnames = FALSE, spacing = 'xs')
  
  output$PlayerH11Data <- renderTable({
    t<-extract_attributes_player(input$ChoixEquipeMaison,input$ChoixEquipeExterieur)
    return(t[c(2:9),11])
  }, colnames = FALSE, spacing = 'xs')
  
})