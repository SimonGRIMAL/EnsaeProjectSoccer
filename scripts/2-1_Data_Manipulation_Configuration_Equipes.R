############################################################################################################################################
# Nouvelle table 'Matchs_Config' 
#
# Construction d'une table 'Matchs_Config' contenant toutes les données de positionnement (XY) des deux équipes. Le programme
# rajoute des variables quantitatives relatives au nombre de joueurs par lignes de jeu (nbY...) ainsi que des variables qualitatives
# 'game_syst_home' et 'game_syst_away' relatives à la description des schémas de jeu des deux équipes.
#
# FONCTION 'Plot_Schemas_Tactiques'
#
# prend en input l'identifiant du match ('match_api_id') et effectue une requête sur la table 'Matchs_Config' pour recuperer les positionnements
# tactiques des deux équipes qu'elle retourne sous forme de schemas.
############################################################################################################################################


library (dplyr)

# Recuperation des donnees de positionnement (X,Y) dans la table 'Matchs_Config'
Matchs_Config <- select(Match, match_api_id, contains("_X"),contains("_Y"))

# Selection des variables d'interet

X_home <- c("home_player_X1", "home_player_X2","home_player_X3","home_player_X4","home_player_X5","home_player_X6",
            "home_player_X7","home_player_X8","home_player_X9","home_player_X10","home_player_X11")
X_away <- c("away_player_X1", "away_player_X2","away_player_X3","away_player_X4","away_player_X5","away_player_X6",
            "away_player_X7","away_player_X8","away_player_X9","away_player_X10","away_player_X11")
Y_home <- c("home_player_Y1", "home_player_Y2","home_player_Y3","home_player_Y4","home_player_Y5","home_player_Y6",
            "home_player_Y7","home_player_Y8","home_player_Y9","home_player_Y10","home_player_Y11")
Y_away <- c("away_player_Y1", "away_player_Y2","away_player_Y3","away_player_Y4","away_player_Y5","away_player_Y6",
            "away_player_Y7","away_player_Y8","away_player_Y9","away_player_Y10","away_player_Y11")

#Calcul du nombre de joueurs par ligne de jeu (home)
Matchs_Config$nbY1_home <- apply(Matchs_Config[,Y_home]==1,MARGIN=1,sum)
Matchs_Config$nbY2_home <- apply(Matchs_Config[,Y_home]==2,MARGIN=1,sum)
Matchs_Config$nbY3_home <- apply(Matchs_Config[,Y_home]==3,MARGIN=1,sum)
Matchs_Config$nbY4_home <- apply(Matchs_Config[,Y_home]==4,MARGIN=1,sum)
Matchs_Config$nbY5_home <- apply(Matchs_Config[,Y_home]==5,MARGIN=1,sum)
Matchs_Config$nbY6_home <- apply(Matchs_Config[,Y_home]==6,MARGIN=1,sum)
Matchs_Config$nbY7_home <- apply(Matchs_Config[,Y_home]==7,MARGIN=1,sum)
Matchs_Config$nbY8_home <- apply(Matchs_Config[,Y_home]==8,MARGIN=1,sum)
Matchs_Config$nbY9_home <- apply(Matchs_Config[,Y_home]==9,MARGIN=1,sum)
Matchs_Config$nbY10_home <- apply(Matchs_Config[,Y_home]==10,MARGIN=1,sum)
Matchs_Config$nbY11_home <- apply(Matchs_Config[,Y_home]==11,MARGIN=1,sum)

# Reduction du nombre de lignes de 11 à 5 (home)
Matchs_Config <- mutate(Matchs_Config, nbY234_home = nbY2_home + nbY3_home + nbY4_home)
Matchs_Config <- mutate(Matchs_Config, nbY56_home =  nbY5_home + nbY6_home)
Matchs_Config <- mutate(Matchs_Config, nbY78_home =  nbY7_home + nbY8_home) 
Matchs_Config <- mutate(Matchs_Config, nbY91011_home = nbY9_home + nbY10_home + nbY11_home)

# Nouvelle variable 'game_syst_home' représentant le schéma tactique de l'équipe 'home'
Matchs_Config <- mutate(Matchs_Config, game_syst_home = paste(nbY1_home,nbY234_home,nbY56_home,nbY78_home,nbY91011_home, sep="_"))

#Calcul du nombre de joueurs par ligne de jeu (away)
Matchs_Config$nbY1_away <- apply(Matchs_Config[,Y_away]==1,MARGIN=1,sum)
Matchs_Config$nbY2_away <- apply(Matchs_Config[,Y_away]==2,MARGIN=1,sum)
Matchs_Config$nbY3_away <- apply(Matchs_Config[,Y_away]==3,MARGIN=1,sum)
Matchs_Config$nbY4_away <- apply(Matchs_Config[,Y_away]==4,MARGIN=1,sum)
Matchs_Config$nbY5_away <- apply(Matchs_Config[,Y_away]==5,MARGIN=1,sum)
Matchs_Config$nbY6_away <- apply(Matchs_Config[,Y_away]==6,MARGIN=1,sum)
Matchs_Config$nbY7_away <- apply(Matchs_Config[,Y_away]==7,MARGIN=1,sum)
Matchs_Config$nbY8_away <- apply(Matchs_Config[,Y_away]==8,MARGIN=1,sum)
Matchs_Config$nbY9_away <- apply(Matchs_Config[,Y_away]==9,MARGIN=1,sum)
Matchs_Config$nbY10_away <- apply(Matchs_Config[,Y_away]==10,MARGIN=1,sum)
Matchs_Config$nbY11_away <- apply(Matchs_Config[,Y_away]==11,MARGIN=1,sum)

# Reduction du nombre de lignes de 11 à 5 (away)
Matchs_Config <- mutate(Matchs_Config, nbY234_away = nbY2_away + nbY3_away + nbY4_away)
Matchs_Config <- mutate(Matchs_Config, nbY56_away = nbY5_away + nbY6_away) 
Matchs_Config <- mutate(Matchs_Config, nbY78_away = nbY7_away + nbY8_away) 
Matchs_Config <- mutate(Matchs_Config, nbY91011_away = nbY9_away + nbY10_away + nbY11_away)

# Nouvelle variable 'game_syst_home' représentant le schéma tactique de l'équipe 'away'
Matchs_Config <- mutate(Matchs_Config, game_syst_away = paste(nbY1_away,nbY234_away,nbY56_away,nbY78_away,nbY91011_away, sep="_"))

# Configuratons de jeu 
table(Matchs_Config$game_syst_home)
table(Matchs_Config$game_syst_away)

# Exploration differentes configurations de matchs
par(mfrow =c(1,2))

Num_Match <- 3000
plot(t(Matchs_Config[Num_Match,X_home]),t(Matchs_Config[Num_Match,Y_home]),pch=15,col="blue",xlab="",ylab="", main=paste("Home team game system :",Matchs_Config$game_syst_home[Num_Match]))
plot(t(Matchs_Config[Num_Match,X_away]),t(Matchs_Config[Num_Match,Y_away]),pch=15,col="red",xlab="",ylab="", main=paste("Away team game system :",Matchs_Config$game_syst_away[Num_Match]))

rm(X_home,X_away,Y_home, Y_away)
####################################################################################################################################################

# La fonction 'Plot_Schemas_Tactiques' prend en input l'identifiant du match (match_api_id) et plotte les schemas tactiques des deux equipes
Match_ID <- 836456

Plot_Schemas_Tactiques <- function(Match_ID){
  Match_Selection <- Matchs_Config %>% filter(match_api_id == Match_ID)
  plot(t(Match_Selection[,X_home]),t(Match_Selection[,Y_home]),pch=15,col="blue",xlab="",ylab="", main=paste("Home team game system :",Match_Selection[,"game_syst_home"]))
  plot(t(Match_Selection[,X_away]),t(Match_Selection[,Y_away]),pch=15,col="red",xlab="",ylab="", main=paste("Away team game system :",Match_Selection[,"game_syst_away"]))
}

Plot_Schemas_Tactiques(662287)

