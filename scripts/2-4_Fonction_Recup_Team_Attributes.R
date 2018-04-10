################################################################################################################################################
# nouvelle fonction 'Date_Corresp'
#
# Dans la table Team_Attributes, la variable date ne comprend que sxi modalités : 2010/02/22, 2011/02/22, 2012/02/22,2013/09/20,2014/09/19 et 2015/09/10
# La fonction Date_Corresp prend en argument une date de match et retourne la derniere date connue (dans la table Team_Attributes) precedant la date du match.
#
# nouvelle Table 'Team_Attributes_Match'
#
# La fonction 'Team_Attributes_MAtch' prend un seul argument : l'identifiant du match. A partir de cet identifiant, la fonction recupere
# la date du match, trouve la date correspondante par appel de la fonction 'Date_Corresp' ainsi que les identifiants des deux equipes.
# La fonction récupère ensuite les caractéristiques des deux équipes et les stocke dans la table 'Team_Attributes'.
#
# Dans la table Team_Attributes, la variable date ne comprend que sxi modalités : 2010/02/22, 2011/02/22, 2012/02/22,2013/09/20,2014/09/19 et 2015/09/10
# La fonction Date_Corresp prend en argument une date de match et retourne la derniere date connue (dans la table Team_Attributes) precedant la date du match.
#
# nouvelle Table 'Team_Attributes_All'
#
# En fin de programme, un appel en  boucle de la fonction 'Recup_Team_Attributes' merge l'ensemble des tables 'Team_Attributes'
# dans une nouvelle table 'Team_Attributes_All'. Attention, ce calcul est assez chronophage (2 heures)
################################################################################################################################################

library(dplyr)
library(parallel)


Date_Corresp <- function(EntryDate){
  if (EntryDate >= as.Date("2015/09/10")){
    ReturnDate <- as.Date("2015/09/10") 
    } else if (EntryDate >= as.Date("2014/09/19")){
      ReturnDate <- as.Date("2014/09/19") 
    } else if (EntryDate >= as.Date("2013/09/20")){
      ReturnDate <- as.Date("2013/09/20") 
    } else if (EntryDate >= as.Date("2012/02/22")){
      ReturnDate <- as.Date("2012/02/22") 
    } else if (EntryDate >= as.Date("2011/02/22")){
      ReturnDate <- as.Date("2011/02/22") 
    } else {ReturnDate <- as.Date("2010/02/22")}
  return(ReturnDate)
}

Date_Corresp("2012/07/29")

Recup_Team_Attributes <- function(match_ID){
  
  # 1- Recuperation date du match
  Match_Date <- t(Match %>% filter(match_api_id==match_ID) %>% select(date))
  # 2- tansformation date du match : appel de la fonction Date_Corresp
  Match_Date <- Date_Corresp(Match_Date)
  # 3- Recuperation des identifiants d'equipes 
  home_ID <- as.numeric(Match %>% filter(match_api_id == match_ID) %>% select(home_team_api_id))
  away_ID <- as.numeric(Match %>% filter(match_api_id == match_ID) %>% select(away_team_api_id))
  # 4- Recuperation des caracteristiques des equipes, rajout de l'identifiant du match
  home_Attributes <- Team_Attributes %>% filter(team_api_id == home_ID & date == Match_Date) %>% mutate(match_api_id = match_ID)%>% select(-id,-team_fifa_api_id,-date) 
  away_Attributes <- Team_Attributes %>% filter(team_api_id == away_ID, date == Match_Date) %>% mutate(match_api_id = match_ID) %>% select(-id,-team_fifa_api_id,-date)
  #5- traitement des lignes manquantes
  if(nrow(home_Attributes)==0) {home_Attributes[1,] <- c(home_ID,rep(NA,21),match_ID)}
  if(nrow(away_Attributes)==0) {away_Attributes[1,] <- c(away_ID,rep(NA,21),match_ID)}
  #6- Reordonnement des colonnes : 
  home_Attributes <- data.frame(c(home_Attributes[,23], home_Attributes[,-23]))
  away_Attributes <- data.frame(c(away_Attributes[,23], away_Attributes[,-23]))
  #7 - Changement nom des colonnes
        # home
  old_names <- colnames(home_Attributes[,-1])
  new_names <- paste(old_names,"home", sep="_")
  names(home_Attributes)<-c("match_api_id",new_names)
        # away
  old_names <- colnames(away_Attributes[,-1])
  new_names <- paste(old_names,"away", sep="_")
  names(away_Attributes)<-c("match_api_id",new_names)
  #8 - merge des deux tables
  Team_Attributes_Match <- left_join(home_Attributes,away_Attributes,by="match_api_id")
  
return(Team_Attributes_Match)
}             
 
# Exemple appel de la fonction pour un seul match
Team_Attributes_Match <- Recup_Team_Attributes(Match$match_api_id[5000])

################################################################################################################################################
# Creation de la table 'Team_Attributes_All : appel en boucle de la fonction 'Recup_Team_Attributes. Le temps de calcul est relativement 
# long malgré laparallélisation des calculs (2H)
################################################################################################################################################

T1=Sys.time()
Number_cores <-detectCores()-1
Clust <- makeCluster(Number_cores)
cpt <-1

Team_Attributes_All <- Recup_Team_Attributes(Match$match_api_id[1])
for (match_i in Match$match_api_id[2:nrow(Match)]) {                            
  Team_Attributes_Match <- Recup_Team_Attributes(match_i)
  Team_Attributes_All <- rbind(Team_Attributes_All,Team_Attributes_Match)
  cpt <-cpt+1
  print(cpt)
} 

stopCluster(Clust)
T2=Sys.time()       
Timediff <- T2-T1
Timediff
                  
                  
                  