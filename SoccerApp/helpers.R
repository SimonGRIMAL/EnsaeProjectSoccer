#Scripts sourcé par l'application ou seront stockées les fonctions

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
# Fonction flitre recupération du Match_id de la table Match_Shiny
##############################################################################

filter_match <- function (nom_table,nom_team_home,nom_team_away){
  
  #MatchID <- nom_table %>% filter(home_team_api_name==nom_team_home | away_team_api_name==nom_team_away) %>% select(match_api_id) PK MARCHE PAS ????
  
  team_home <- filter(nom_table,home_team_api_name==nom_team_home)
  Match <- filter(team_home, away_team_api_name==nom_team_away)

  return (Match)
}



##############################################################################
# Fonction mise en forme attrituts de l'�quipe
##############################################################################


mef_attributes_team <- function (nom_table,where_team){

    t_nom_table<-as.data.frame(t(nom_table [,51:59]))
    t_nom_table$v<-rownames(t_nom_table)
    colnames(t_nom_table) <- c(paste("val",where_team,sep='_'),"var")
    t_nom_table<-t_nom_table[,c(2,1)]
    return(t_nom_table)
}