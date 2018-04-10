##############################################################################################################################################
# fonction 'Recup_Player_Attributes et nouvelle Table 'Player_Attributes_Match' (22*45)
#
# La fonction 'Recup_Player_Attributes' prend un seul argument : l'identifiant du match. A partir de cet identifiant, la fonction recupere
# la date du match ainsi que la liste des 22 joueurs presents sur le terrain. Elle recupere ensuite grace a une boucle l'ensemble des
# caracteristiques des 22 joueurs (45 variables) a la derniere date disponible précédant celle du match. Les données sont stockées dans la 
# table 'Player_Attributes_Match'
#
# nouvelle Table 'Player_Attributes_All' (571538*44)
# En fin de programme, un appel en  boucle de la fonction 'Player_Attributes_Match' merge l'ensemble des tables 'Player_Attributes_Match'
# dans une nouvelle table 'Player_Attributes_All. Ce calcul est assez chronophage (9 heures)
##############################################################################################################################################


library(dplyr)
library(parallel)

Recup_Player_Attributes <- function(match_ID){
  
  # 1- Recuperation date du match
  Match_Date <- t(Match %>% filter(match_api_id==match_ID) %>% select(date))
  # 2- Recuperation liste des 22 joueurs du match
  Match_Joueurs <- t(Match %>% filter(match_api_id==match_ID) %>% select(home_player_1:away_player_11))
  # 3- Boucle permettant de recuperer les caracteristiques des 22 joueurs a la premiere date disponible precedant la date du match
  Player_Attributes_Match <- Player_Attributes[1:22,]
  #Player_Attributes_Match[,] <- NA
  for(i in 1:22){
    Play_Att <-  Player_Attributes %>% filter(player_api_id == Match_Joueurs[i]) %>%
                                       filter(date <= Match_Date) %>%
                                       filter(date == max(date))
    
    # gestion des lignes vides
    ifelse((nrow(Play_Att)==0),Player_Attributes_Match[i,]<-NA,Player_Attributes_Match[i,]<-Play_Att)
  }
  # 4- Rajout identifiant de match et des equipes (home/away)
  players <- c(paste("home_player",1:11),paste("away_player",1:11))
  home_away <- c(rep("home",11),rep("away",11))
  Player_Attributes_Match <- Player_Attributes_Match %>% mutate(match_api_id = match_ID, players = players, home_away = home_away)
  
  return(Player_Attributes_Match)
}
# Exemple appel de la fonction pour un seul match
Player_Attributes_Match <- Recup_Player_Attributes(838798)





################################################################################################################################################
# Creation de la table 'Player_Attributes_All : appel en boucle de la fonction 'Recup_Player_Attributes. Le temps de calcul est relativement 
# long malgré laparallélisation des calculs (9H)
###############################################################################################################################################

T1=Sys.time()
Number_cores <-detectCores()-1
Clust <- makeCluster(Number_cores)
cpt <-1

Player_Attributes_All <- Recup_Player_Attributes(Match$match_api_id[1])
for (match_i in Match$match_api_id[2:nrow(Match)]) {                            
  Player_Attributes_Match <- Recup_Player_Attributes(match_i)
  Player_Attributes_All <- rbind(Player_Attributes_All,Player_Attributes_Match)
  cpt <-cpt+1
  print(cpt)
} 

stopCluster(Clust)
T2=Sys.time()       

Timediff <- T2-T1

rm(Player_Attributes_Match,T1,T2,Timediff)                   



