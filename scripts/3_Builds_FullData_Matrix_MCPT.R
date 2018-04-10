##################################################################################################################################################
#
# nouvelle table 'MCPT' : Matchs + Config + Players + Team (20463*105)
#
# Cette nouvelle table represente agrége pour tous les matchs, l'ensemble des données disponibles dans les tables 'Matchs', 'Matchs_Config',
# 'Players_Attributes_Summary_All' et 'Team_Attributes_Summary_All'. Un retraitement préalable est effectué pour retires les variables insuffisament
# renseignées ou redondantes.


library(dplyr)

# les variables PSH, PSD et PSA ont trop de donnees manquantes, elles sont donc supprimees
MCPT_Matchs <- Match[,c("match_api_id","season","goal_diff","B365H","B365D","B365A","BWH","BWD","BWA","IWH","IWD","IWA","LBH","LBD","LBA")]

# Les variables nbY56 et nbY78 sont supprimees pour des questions d'identifiabilité. Les variables relatives aux systemes de jeux 'game_syst'
# ne sont pas prises en compte car le nombre de modalités est trop important.
MCPT_Config <- Matchs_Config[,c("match_api_id", "nbY234_home","nbY91011_home","nbY234_away","nbY91011_away")]

# La plupart des variables représentent un recodage qualitatif de variables quantitatives et sont par conséquent redondantes. Nous ne retenons 
# que les variables qualitatives

MCPT_Team <- Team_Attributes_All[,c("match_api_id","buildUpPlaySpeed_home" ,"buildUpPlayPassing_home","chanceCreationPassing_home",
                                    "chanceCreationCrossing_home","chanceCreationShooting_home","defencePressure_home","defenceAggression_home",
                                    "defenceTeamWidth_home","buildUpPlaySpeed_away","buildUpPlayPassing_away","chanceCreationPassing_away",
                                    "chanceCreationCrossing_away","chanceCreationShooting_away","defencePressure_away","defenceAggression_away",
                                    "defenceTeamWidth_away")]

MCPT_Players <- Player_Attributes_Summary_All

# left Join des 4 tables suivant la cle 'match_api_id'
MCPT <- left_join(MCPT_Matchs, MCPT_Config, by = "match_api_id")
MCPT <- left_join(MCPT, MCPT_Team, by = "match_api_id")
MCPT <- left_join(MCPT, MCPT_Players, by = "match_api_id")

rm(MCPT_Matchs,MCPT_Config,MCPT_Team,MCPT_Players)

# On cree deux  variables explicatives goal_diff_class2 (2 modalites) et goal_diff_class3 (3 modalites)
library(dplyr)
MCPT <- MCPT %>% mutate(goal_diff_class2 = ifelse(goal_diff > 0,"1home_win","2home_draworloss"))
MCPT <- MCPT %>% mutate(goal_diff_class3 = ifelse(goal_diff > 0,"1home_win",ifelse(goal_diff == 0,"2draw","3home_loss")))
MCPT$goal_diff_class2 <- as.factor(MCPT$goal_diff_class2)
levels(MCPT$goal_diff_class2) <-c("1home_win","2home_draworloss")
MCPT$goal_diff_class3 <- as.factor(MCPT$goal_diff_class3)
levels(MCPT$goal_diff_class3) <- c("1home_win","2draw","3home_loss")

#Reordonnement des colonnes
MCPT <- MCPT[,c(1:3,(ncol(MCPT)-1):ncol(MCPT),4:(ncol(MCPT)-2))]

# On retire les lignes contenant des NA
MCPT <- na.omit(MCPT)

