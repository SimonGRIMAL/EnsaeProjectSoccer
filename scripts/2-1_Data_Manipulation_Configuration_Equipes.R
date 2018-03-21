
# Differentes configurations de match : home_team & away_team

Matchs_Config <- Match[,c(1:11,116:117,12:55)]

# Vu le tres grand nombre de configurations de jeu obtenu en permutant eventuellement les joueurs d'une meme ligne
# nous choisissons de denombrer le nombre de joueurs par ligne (11 lignes au total)

# home team (11 lignes de jeu)

Matchs_Config$nbY1_home <- apply(Matchs_Config[,36:46]==1,MARGIN=1,sum)
Matchs_Config$nbY2_home <- apply(Matchs_Config[,36:46]==2,MARGIN=1,sum)
Matchs_Config$nbY3_home <- apply(Matchs_Config[,36:46]==3,MARGIN=1,sum)
Matchs_Config$nbY4_home <- apply(Matchs_Config[,36:46]==4,MARGIN=1,sum)
Matchs_Config$nbY5_home <- apply(Matchs_Config[,36:46]==5,MARGIN=1,sum)
Matchs_Config$nbY6_home <- apply(Matchs_Config[,36:46]==6,MARGIN=1,sum)
Matchs_Config$nbY7_home <- apply(Matchs_Config[,36:46]==7,MARGIN=1,sum)
Matchs_Config$nbY8_home <- apply(Matchs_Config[,36:46]==8,MARGIN=1,sum)
Matchs_Config$nbY9_home <- apply(Matchs_Config[,36:46]==9,MARGIN=1,sum)
Matchs_Config$nbY10_home <- apply(Matchs_Config[,36:46]==10,MARGIN=1,sum)
Matchs_Config$nbY11_home <- apply(Matchs_Config[,36:46]==11,MARGIN=1,sum)

# home team (5 lignes de jeu)

Matchs_Config$nbY234_home <- Matchs_Config$nbY2_home + Matchs_Config$nbY3_home + Matchs_Config$nbY4_home
Matchs_Config$nbY56_home <-  Matchs_Config$nbY5_home + Matchs_Config$nbY6_home 
Matchs_Config$nbY78_home <-  Matchs_Config$nbY7_home + Matchs_Config$nbY8_home 
Matchs_Config$nbY91011_home <- Matchs_Config$nbY9_home + Matchs_Config$nbY10_home + Matchs_Config$nbY11_home

Matchs_Config$game_syst_home <- paste(Matchs_Config$nbY1_home,Matchs_Config$nbY234_home,Matchs_Config$nbY56_home,
                                      Matchs_Config$nbY78_home,Matchs_Config$nbY91011_home, sep="_")

# away team (11 lignes de jeu)

Matchs_Config$nbY1_away <- apply(Matchs_Config[,47:57]==1,MARGIN=1,sum)
Matchs_Config$nbY2_away <- apply(Matchs_Config[,47:57]==2,MARGIN=1,sum)
Matchs_Config$nbY3_away <- apply(Matchs_Config[,47:57]==3,MARGIN=1,sum)
Matchs_Config$nbY4_away <- apply(Matchs_Config[,47:57]==4,MARGIN=1,sum)
Matchs_Config$nbY5_away <- apply(Matchs_Config[,47:57]==5,MARGIN=1,sum)
Matchs_Config$nbY6_away <- apply(Matchs_Config[,47:57]==6,MARGIN=1,sum)
Matchs_Config$nbY7_away <- apply(Matchs_Config[,47:57]==7,MARGIN=1,sum)
Matchs_Config$nbY8_away <- apply(Matchs_Config[,47:57]==8,MARGIN=1,sum)
Matchs_Config$nbY9_away <- apply(Matchs_Config[,47:57]==9,MARGIN=1,sum)
Matchs_Config$nbY10_away <- apply(Matchs_Config[,47:57]==10,MARGIN=1,sum)
Matchs_Config$nbY11_away <- apply(Matchs_Config[,47:57]==11,MARGIN=1,sum)

# away team (5 lignes de jeu)

Matchs_Config$nbY234_away <- Matchs_Config$nbY2_away + Matchs_Config$nbY3_away + Matchs_Config$nbY4_away
Matchs_Config$nbY56_away <-  Matchs_Config$nbY5_away + Matchs_Config$nbY6_away 
Matchs_Config$nbY78_away <-  Matchs_Config$nbY7_away + Matchs_Config$nbY8_away 
Matchs_Config$nbY91011_away <- Matchs_Config$nbY9_away + Matchs_Config$nbY10_away + Matchs_Config$nbY11_away

Matchs_Config$game_syst_away <- paste(Matchs_Config$nbY1_away,Matchs_Config$nbY234_away,Matchs_Config$nbY56_away,
                                      Matchs_Config$nbY78_away,Matchs_Config$nbY91011_away, sep="_")



# Configuratons de jeu 
glimpse(Matchs_Config$game_syst_home)
glimpse(Matchs_Config$game_syst_away)
table(Matchs_Config$game_syst_home)
table(Matchs_Config$game_syst_away)



# Exploration differentes configurations de matchs
par(mfrow =c(1,2))

Num_Match <- 3000
plot(t(Matchs_Config[Num_Match,14:24]),t(Matchs_Config[Num_Match,36:46]),pch=15,col="blue",xlab="",ylab="", main=paste("Home team game system :",Matchs_Config$game_syst_home[Num_Match]))
plot(t(Matchs_Config[Num_Match,25:35]),t(Matchs_Config[Num_Match,47:57]),pch=15,col="red",xlab="",ylab="", main=paste("Away team game system :",Matchs_Config$game_syst_away[Num_Match]))

# Les differentes configurations de jeux ont elles un pouvoir predictif ? le signe de coefficients est encourageant.

mod_linear <- lm(goal_diff~nbY91011_home+nbY91011_away+nbY78_home+nbY78_away+
                  nbY56_home+nbY56_away+nbY234_home+nbY234_away, data=Matchs_Config)

summary(mod_linear)

library(nnet)

Matchs_Config$goal_diff_class <- relevel(Matchs_Config$goal_diff_class, ref = "draw")
mod_multinom <- multinom(goal_diff_class~nbY91011_home+nbY91011_away+nbY78_home+nbY78_away+
                    nbY56_home+nbY56_away+nbY234_home+nbY234_away, data=Matchs_Config)
summary(mod_multinom)
