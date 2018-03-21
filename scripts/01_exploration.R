############################
# enelver les NA
############################

match2 <- match[complete.cases(match),]
# 1 762 observations au lieu de 25 979 !

match3<-match[is.na(match$home_player_1)==FALSE,]
# 24 755 observations au lieu de 25 979 

############################
# exemple d'un joueur
############################

test_match <- match3[match3$home_player_1==39890,]
#3 matchs 

test_player <- player[player$player_api_id==39890,]
# 1 ligne 

test_player_att <- player_attributes[player_attributes$player_api_id==39890,]
#10 lignes car 10 dates

test_team<-team[team$team_api_id==9997,]
#1 ligne

test_team_att<-team_attributes[team_attributes$team_api_id==9997,]
# 3 lignes car 3 dates


############################
# calcul identifiant unique
############################

length(unique(player$player_api_id))
#11 060 = pas de doublons

length(unique(player$player_fifa_api_id))
#11 060 = pas de doublons

length(unique(player_attributes$player_api_id))
#11 060 comme la table des joueurs

length(unique(player_attributes$player_fifa_api_id))
#11 062 vs 11 060 pour la table des joueurs


length(unique(team$team_api_id))
# 299 = pas de doublons
length(unique(team_attributes$team_api_id))
#288 vs 299 equipes


###################################
# table match
###################################

names(match)


# valeur manquante sur les id equipes, et joueurs

length(is.na(match$home_team_api_id)==FALSE)
#25 979

length(is.na(match$home_team_api_id)==TRUE)
#25 979 

length(match$home_team_api_id==9997)
#25 979
length(isTRUE(match$home_team_api_id==9997))
#1


test<-match$home_team_api_id[is.na(match$home_team_api_id)]
#0
test<-match$away_team_api_id[is.na(match$away_team_api_id)]
#0


length(match$home_player_1[is.na(match$home_player_1)])
#1 224
length(match$home_player_2[is.na(match$home_player_2)])
#1 315
length(match$home_player_3[is.na(match$home_player_3)])
#1 281
length(match$home_player_4[is.na(match$home_player_4)])
#1 323
length(match$home_player_5[is.na(match$home_player_5)])
#1 316
length(match$home_player_6[is.na(match$home_player_6)])
#1 325
length(match$home_player_7[is.na(match$home_player_7)])
#1 227
length(match$home_player_8[is.na(match$home_player_8)])
#1 309
length(match$home_player_9[is.na(match$home_player_9)])
#1 273
length(match$home_player_10[is.na(match$home_player_10)])
#1 436
length(match$home_player_11[is.na(match$home_player_11)])
#1 555


match_hna <- match[complete.cases(match[,c(56:77)]),]
#21 374 match avec tous les joueurs renseignÃ©s (soit 82%)

match_hna_2 <- match_hna[complete.cases(match_hna[,c(12:55)]),]
#21 361 match avec toutes les coordonnÃ©es des joueurs renseignÃ©s      

# exploiter les coordonnées en tant que "dispositif de jeu" pour la modelisation ?

# coordonÃ©es des joueurs sur match avec tous les joueurs renseignÃ©s              
stat <- summary(match_hna_2[,c(12:55)])
print(stat)
# tous les X1 et Y1 sont égaux à 1.




# stats par saison et ligue
table(match$league_id)

#   1  1729  4769  7809 10257 13274 15722 17642 19694 21518 24558 
#1728  3040  3040  2448  3017  2448  1920  2052  1824  3040  1422 

table(match$season)

#2008/2009 2009/2010 2010/2011 2011/2012 2012/2013 2013/2014 2014/2015 2015/2016 
#   3326      3230      3260      3220      3260      3032      3325      3326 

table(match$league_id,match$season)

table(match$stage)


###################################
# table player et player attributes
###################################

summary(player)
# unité du poids ? livre à convertir en kg au moins pour une eventuelle restit

summary(player_attributes)
# 836 NA sur la plupart des variables, 2 713 sur d'autres
# cb de joueurs sont concernés ?

###################################
# table team et team attributes
###################################

summary(team)

summary(team_attributes)
# pas de NA


##########################################
# comment traiter les données des joueurs
#le numéro du joueur (1 à 11) doit il compter ??
#########################################






