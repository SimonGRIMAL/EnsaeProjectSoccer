library(dplyr)


Recup_Player_Attributes <- function(match_ID){
  
  # 1- Recuperation date du match
  Match_date <- t(Match %>% filter(match_api_id==match_ID) %>% select(date))
  # 2- Recuperation liste des 22 joueurs du match
  Match_Joueurs <- t(Match %>% filter(match_api_id==match_ID) %>% select(home_player_1:away_player_11))
  
  # 3- Boucle permettant de recuperer les caracteristiques des 22 joueurs a la premiere date disponible precedant la date du match

  Player_Attributes_match <- Player_Attributes[1:22,]
  Player_Attributes_match[,] <- 'NA'
  for(i in 1:22){
    Player_Attributes_match[i,] <-  Player_Attributes %>%
                                  filter(player_api_id==Match_Joueurs[i]) %>% 
                                  filter(date <= Match_date) %>%
                                  filter(date == max(date)) 
  }
  rm(Match_date,Match_Joueurs)
  return(Player_Attributes_match)
}
                            
Player_Attributes_match <- Recup_Player_Attributes(1989836)
    
                            


