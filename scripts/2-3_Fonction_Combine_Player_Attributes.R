#############################################################################################################################################
# fonction 'Combine_Player_Attributes' et nouvelle Table 'Player_Attributes_Summary' (1*71)
#
# Tout comme la fonction 'Recup_Player_Attribbutes', la fonction 'Combine_Player_Attributes' prend un seul argument : l'identifiant du match. 
# A partir de cet identifiant, la fonction recupere la date du match ainsi que la liste des 22 joueurs presents sur le terrain. Elle 
# recupere ensuite grace a une boucle l'ensemble des caracteristiques des 22 joueurs (42 variables) dans la table 'Player_Attributes' 
# a la derniere date disponible précédant celle du match. 
# A la difference de la fonction 'Recup_Player_Attributes', la fonction 'Combine_Player_Attribute' agrege (moyenne arithmetique) ensuite les 
# carateristiques des joueurs pour chacune des deux equipes et cree pour chaque match un resume des caracteristiques des equipes home et away
# stocke dans la table 'Player_Attributes_Summary'.
#
# nouvelle Table 'Player_Attributes_All' (25979*71)
# En fin de programme, un appel en  boucle de la fonction 'Combine_Player_Attributes' merge l'ensemble des tables 'Player_Attributes_Summary'
# dans une nouvelle table 'Player_Attributes_All'. Attention, cee calcul est assez chronophage (5 heures)
##############################################################################################################################################

library(dplyr)
library(stringr)

Combine_Player_Attributes <- function(match_ID){
  
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
  home_away <- c(rep("home",11),rep("away",11))
  Player_Attributes_Match <- Player_Attributes_Match %>% mutate(match_api_id = match_ID, home_away = home_away)
  # 5- Reordonnement colonnes
  Player_Attributes_Match <- data.frame(c(Player_Attributes_Match[,43:44], Player_Attributes_Match[,-(43:44)]))
  # 6- Summary : des caracteristiques moyennes sont calculees pour chacune des deux equipes
  Player_Attributes_Summary <- Player_Attributes_Match %>% 
    select(-id,-player_fifa_api_id,-date,-preferred_foot,-attacking_work_rate,-defensive_work_rate)%>%
    group_by(match_api_id,home_away) %>%
    summarise(overall_rating = mean(overall_rating,na.rm=TRUE),
              potential = mean(potential,na.rm=TRUE),
              crossing = mean(crossing,na.rm=TRUE),
              finishing = mean(finishing,na.rm=TRUE),
              heading_accuracy = mean(heading_accuracy,na.rm=TRUE),
              short_passing = mean(short_passing,na.rm=TRUE),
              volleys = mean(volleys,na.rm=TRUE),
              dribbling = mean(dribbling,na.rm=TRUE),
              curve = mean(curve,na.rm=TRUE),
              free_kick_accuracy = mean(free_kick_accuracy,na.rm=TRUE),
              long_passing = mean(long_passing,na.rm=TRUE),
              ball_control = mean(ball_control,na.rm=TRUE),
              acceleration = mean(acceleration,na.rm=TRUE),
              sprint_speed = mean(sprint_speed,na.rm=TRUE),
              agility = mean(agility,na.rm=TRUE),
              reactions = mean(reactions,na.rm=TRUE),
              balance = mean(balance,na.rm=TRUE),
              shot_power = mean(shot_power,na.rm=TRUE),
              jumping = mean(jumping,na.rm=TRUE),
              stamina = mean(stamina,na.rm=TRUE),
              strength = mean(strength,na.rm=TRUE),
              long_shots = mean(long_shots,na.rm=TRUE),
              aggression = mean(aggression,na.rm=TRUE),
              interceptions = mean(interceptions,na.rm=TRUE),
              positioning = mean(positioning,na.rm=TRUE),
              vision = mean(vision,na.rm=TRUE),
              penalties = mean(penalties,na.rm=TRUE),
              marking = mean(marking,na.rm=TRUE),
              standing_tackle = mean(standing_tackle,na.rm=TRUE),
              sliding_tackle = mean(sliding_tackle,na.rm=TRUE),
              gk_diving = mean(gk_diving,na.rm=TRUE),
              gk_handling = mean(gk_handling,na.rm=TRUE),
              gk_kicking = mean(gk_kicking,na.rm=TRUE),
              gk_positioning = mean(gk_positioning,na.rm=TRUE),
              gk_reflexe = mean(gk_reflexes,na.rm=TRUE))
 
  # 7- On cree un jeu de variables pour l'equipe 'home'
  Player_Attributes_Sum_home <- Player_Attributes_Summary %>% filter(home_away=="home")
  old_names <- colnames(Player_Attributes_Sum_home[,-1])
  new_names <- paste(old_names,"home", sep="_")
  names(Player_Attributes_Sum_home)<-c("match_api_id",new_names) 
  # 8- On cree un jeu de variables pour l'equipe 'home'
  Player_Attributes_Sum_away <- Player_Attributes_Summary %>% filter(home_away=="away")
  old_names <- colnames(Player_Attributes_Sum_away[,-1])
  new_names <- paste(old_names,"away", sep="_")
  names(Player_Attributes_Sum_away)<-c("match_api_id",new_names)
  # 9- On regroupe les deux jeux de donnees en une seule table
  Player_Attributes_Summary <- left_join(Player_Attributes_Sum_home,Player_Attributes_Sum_away,by="match_api_id")
  Player_Attributes_Summary <- Player_Attributes_Summary %>% select(-home_away_home,-home_away_away)
  rm(Player_Attributes_Sum_away,Player_Attributes_Sum_home)
  
  return(Player_Attributes_Summary)
}

# Exemple appel de la fonction pour un seul match
Player_Attributes_Summary <- Combine_Player_Attributes(838798)

################################################################################################################################################
# Creation de la table 'Player_Attributes_Summary_All : appel en boucle de la fonction 'Combine_Player_Attributes. Le temps de calcul est relativement 
# long malgré laparallélisation des calculs (5H)
###############################################################################################################################################

# Exemple appel de la fonction en boucle et mesure du temps de calcul

T1=Sys.time()
Number_cores <-detectCores()-1
Clust <- makeCluster(Number_cores)
cpt <-1

Player_Attributes_Summary_All <- Combine_Player_Attributes(Match$match_api_id[1])
for (match_i in Match$match_api_id[2:nrow(Match)]) {                            
  Player_Attributes_Summary <- Combine_Player_Attributes(match_i)
  Player_Attributes_Summary_All <- rbind(Player_Attributes_Summary_All,Player_Attributes_Summary)
  cpt <-cpt+1
  print(cpt)
} 

stopCluster(Clust)
T2=Sys.time()       
Timediff <- T2-T1
Timediff



