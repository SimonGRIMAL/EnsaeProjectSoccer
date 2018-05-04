#Scripts sourc√© par l'application ou seront stock√©es les fonctions

#Exemple
# ExempleDeFonction <- function(Entree1, Entree2){
#   
#   #ce que fait ma fonction
#   
#   return(SortieDeMaFonction)
# }



##############################################################################
# Fonction flitre sur les equipes + variables graphiques
##############################################################################

filter_team <- function (nom_table,nom_team){
  
  selected_team <- filter(nom_table,team_long_name==nom_team)
  
  selected_team_g<-melt(selected_team[,c("nb_goal_scored_2008/2009","nb_goal_scored_2009/2010",
                        "nb_goal_scored_2010/2011","nb_goal_scored_2011/2012",
                        "nb_goal_scored_2012/2013","nb_goal_scored_2013/2014",
                        "nb_goal_scored_2014/2015",
                        "nb_goal_conceded_2008/2009","nb_goal_conceded_2009/2010",
                        "nb_goal_conceded_2010/2011","nb_goal_conceded_2011/2012",
                        "nb_goal_conceded_2012/2013","nb_goal_conceded_2013/2014",
                        "nb_goal_conceded_2014/2015"
                        )])
  
  return (selected_team_g)
}

#test<- filter_team(Team_home_viz,"Paris Saint-Germain")


##############################################################################
# Fonction flitre recup√©ration du Match_id de la table Match_Shiny
##############################################################################

filter_match <- function (nom_table,nom_team_home,nom_team_away){
  
  #MatchID <- nom_table %>% filter(home_team_api_name==nom_team_home | away_team_api_name==nom_team_away) %>% select(match_api_id) PK MARCHE PAS ????
  
  team_home <- filter(nom_table,home_team_api_name==nom_team_home)
  Match <- filter(team_home, away_team_api_name==nom_team_away)

  return (Match)
}



##############################################################################
# Fonction extraction attributs de l'Èquipe
##############################################################################


extract_attributes_team <- function (nom_table,where_team){

    t_nom_table<-as.data.frame(t(nom_table [,51:59]))
    t_nom_table$v<-c("Build up play speed","Build up play dribbling","Build up play passing","Chance creation passing","Chance creation crossing","Chance creation shooting","Defence pressure","Defence aggression","Defence team width")
    colnames(t_nom_table) <- c(paste("val",where_team,sep='_'),"var")
    t_nom_table<-t_nom_table[,c(2,1)]
    return(t_nom_table)
}


##############################################################################
# Fonction mise en forme attributs de l'Èquipe
##############################################################################

mef_attributes_team <- function(home,away) {
  
  mix<- home %>% left_join (away)
  
  for(i in 1:9) {
    if (mix[i,2]> mix[i,3]) { 
      mix[i,2]<-paste('<div style="color: blue; font-weight: bold; position:absolute"> <span>',mix[i,2], '</span></div>')
    }
    else {
      mix[i,3]<-paste('<div style="color: red; font-weight: bold; position:absolute"> <span>',mix[i,3], '</span></div>')
    }
  }
  return(mix)
}

##############################################################################
# Fonction extraction Attributs des joueurs
##############################################################################

extract_attributes_player<-function (nom_team_home,nom_team_away){
  
  players <- filter_match(Match_Shiny,nom_team_home,nom_team_away)
  players<- as.data.frame(t(players[,c(56:77)]))
  colnames(players)<-"player_api_id"
  
  players<-players%>%inner_join(Player_viz,by=c("player_api_id"))
  
  players<-as.data.frame(t(players))
  
}

