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

