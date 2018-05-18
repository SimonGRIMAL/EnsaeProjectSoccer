
##############################################################################################################################################
# Train & Test sets
##############################################################################################################################################

unique(MCPT$season)
train <- MCPT$season %in% c("2008/2009","2010/2011","2012/2013","2014/2015")
sum(train)
test <- MCPT$season %in% c("2009/2010","2011/2012","2013/2014","2015/2016")
sum(test)

###############################################################################################################################################
#  ACP sur les donnees de Bookmakers
###############################################################################################################################################
# 97% de l'inertie des 10 variables de bookmakers est résumée dans le premier plan factoriel (2 dimensions)   
# Les cotes des 4 bookmakers sont tres correles entre elles et peuvent par consequent etre agregees


library(FactoMineR)
mydata <-MCPT[,c("B365H","B365D","B365A","BWH","BWD","BWA","IWH","IWD","IWA","LBH","LBD","LBA")]

# Matrices de correlation 
cor(mydata[,c("B365H","BWH","IWH","LBH")])
cor(mydata[,c("B365D","BWD","IWD","LBD")])
cor(mydata[,c("B365A","BWA","IWA","LBA")])

# ACP
pca_bookmakers <-PCA(mydata[train,],scale.unit=TRUE,graph=TRUE)
summary(pca_bookmakers,nbelements=ncol(mydata))
names(pca_bookmakers)
barplot(pca_bookmakers$eig[,2],names=paste("Dim",1:nrow(pca_bookmakers$eig)))

###############################################################################################################################################
#  ACP sur les donnees 'Team Attributes'
###############################################################################################################################################

# donnees 'Team'
mydata <- MCPT[,c("buildUpPlaySpeed_home" ,"buildUpPlayPassing_home","chanceCreationPassing_home",
                  "chanceCreationCrossing_home","chanceCreationShooting_home","defencePressure_home","defenceAggression_home",
                  "defenceTeamWidth_home","buildUpPlaySpeed_away","buildUpPlayPassing_away","chanceCreationPassing_away",
                  "chanceCreationCrossing_away","chanceCreationShooting_away","defencePressure_away","defenceAggression_away",
                  "defenceTeamWidth_away")]

# ACP : donnees 'Team'
pca_team <-PCA(mydata[train,],scale.unit=TRUE,graph=TRUE)
summary(pca_team,nbelements=ncol(mydata))
barplot(pca_team$eig[,2],names=paste("Dim",1:nrow(pca_team$eig)))

# donnees 'Team diff'
mydata_diff <- mydata %>% 
  
  mutate(buildUpPlaySpeed_diff = buildUpPlaySpeed_home - buildUpPlaySpeed_away,
                                 buildUpPlayPassing_diff = buildUpPlayPassing_home -buildUpPlayPassing_away,
                                 chanceCreationPassing_diff = chanceCreationPassing_home -chanceCreationPassing_away,
                                 chanceCreationCrossing_diff = chanceCreationCrossing_home - chanceCreationCrossing_away,
                                 chanceCreationShooting_diff = chanceCreationShooting_home - chanceCreationShooting_away,
                                 defencePressure_diff = defencePressure_home -defencePressure_away,
                                 defenceAggression_diff = defenceAggression_home - defenceAggression_away,
                                 defenceTeamWidth_diff = defenceTeamWidth_home - defenceTeamWidth_away) %>%
                                
  select(buildUpPlaySpeed_diff,buildUpPlayPassing_diff,chanceCreationPassing_diff,
                                       chanceCreationCrossing_diff,chanceCreationShooting_diff,defencePressure_diff,
                                       defenceAggression_diff,defenceTeamWidth_diff)


# ACP donnees Team diff
pca_team_diff <-PCA(mydata_diff[train,],scale.unit=TRUE,graph=TRUE)
summary(pca_team_diff,nbelements=ncol(mydata_diff))
barplot(pca_team_diff$eig[,2],names=paste("Dim",1:nrow(pca_team_diff$eig)))

###############################################################################################################################################
#  ACP sur les donnees 'players Attributes'
###############################################################################################################################################

mydata <- Player_Attributes_Summary_All[,-1]

# ACP
pca_players <-PCA(mydata[train,],scale.unit=TRUE,graph=TRUE)
summary(pca_players,nbelements=ncol(mydata))
barplot(pca_players$eig[,2],names=paste("Dim",1:nrow(pca_players$eig)))

