
library(dplyr)

install.packages("reshape")
install.packages("reshape2")
library(reshape2)
library(reshape)

library(ggplot2)

table(Match$season)

# match saison 2015/2016 hors championnat de Pologne 
Match_viz=filter(Match, season=="2015/2016" & league_id != 15722)


table(Match_viz$league_id)

#comptage equipes

length(unique(Match_viz$home_team_api_id))
#172 equipe home
length(unique(Match_viz$away_team_api_id))
#172 equipe away

###############
# EQUIPE HOME
################

list_home_team_viz<-as.data.frame(unique(Match_viz$home_team_api_id))
colnames(list_home_team_viz) <- "home_team_api_id"

nb_match_home<-count(Match_viz,home_team_api_id)
table(nb_match_home$n)
#15 17 18 19 
#16 54 10 92 
  
#cb de matchs entre les deux mêmes equipes
nb_match_home_away<- Match_viz %>% group_by(home_team_api_id) %>%count(away_team_api_id)
table(nb_match_home_away$n)

#   1    2    3 
#2717  180    3 

#exemple
filter(nb_match_home_away,n==2)
      
test<-filter(Match_viz, home_team_api_id==7896  & away_team_api_id==9956)
test2<-filter(Match_viz, (home_team_api_id==7896  & away_team_api_id==9956) | (home_team_api_id==9956  & away_team_api_id==7896))


## nombre de match disputés à domicile par equipe et par saison

nb_match_team_season<- Match %>% group_by(home_team_api_id,season) %>% count()
Team_home_viz<-cast(nb_match_team_season,home_team_api_id~season,fill=0)
colnames(Team_home_viz)[2:9]<-paste("nb_match", colnames(Team_home_viz[2:9]), sep = "_")


## nb de victoires, défaites, matchs nuls

#creation variable victoire, défaite, match nul
Match$goal_diff <- Match$home_team_goal-Match$away_team_goal
Match$goal_diff_class <- cut(Match$goal_diff, breaks=c(min(Match$goal_diff),-1e-10,+1e-10,max(Match$goal_diff)))
levels(Match$goal_diff_class) <- c("home_loss","draw","home_win")

nb_match_team_season_2<- Match %>% group_by(home_team_api_id,season,goal_diff_class) %>% count()

Team_home_viz_2<-cast(nb_match_team_season_2,home_team_api_id~season+goal_diff_class,fill=0)
#colnames(Team_home_viz_2)
#table(Team_home_viz_2$'2015/2016_NA')
colnames(Team_home_viz_2)[2:26]<-paste("nb_match", colnames(Team_home_viz_2[2:26]), sep = "_")

## nb de buts marqués et encaissés
nb_match_team_season_3<- Match %>% group_by(home_team_api_id,season) %>% summarize(goal_scored=sum(home_team_goal), goal_conceded=sum(away_team_goal))

Team_home_viz_31<-cast(nb_match_team_season_3,home_team_api_id~season,value="goal_scored",fill=0)
colnames(Team_home_viz_31)[2:9]<-paste("nb_goal_scored", colnames(Team_home_viz_31[2:9]), sep = "_")

Team_home_viz_32<-cast(nb_match_team_season_3,home_team_api_id~season,value="goal_conceded",fill=0)
colnames(Team_home_viz_32)[2:9]<-paste("nb_goal_conceded", colnames(Team_home_viz_32[2:9]), sep = "_")

#jointures avec join de dplyr
tt<-list_home_team_viz %>% inner_join(Team_home_viz)%>% inner_join (Team_home_viz_2) %>% inner_join (Team_home_viz_31)%>% inner_join (Team_home_viz_32)
Team_home_viz<-tt


#jointure avec team attributes
temp <- list_home_team_viz %>% inner_join(Team_Attributes,by= c("home_team_api_id"="team_api_id"))
# 905

table(temp$date)
temp$annee<-substr(temp$date,1,4)
table(temp$annee)

#2010 2011 2012 2013 2014 2015 
# 142  147  146  152  155  163 

colnames(temp)

# variables à restituer

#buildUpPlaySpeedClass
#buildUpPlayDribblingClass
#buildUpPlayPassingClass
#buildUpPlayPositioningClass
#chanceCreationPassingClass
#chanceCreationCrossingClass
#chanceCreationShootingClass
#chanceCreationPositioningClass
#defencePressureClass
#defenceAggressionClass
#defenceTeamWidthClass
#defenceDefenderLineClass

#ces données evoluent au cours du temps ? 

test<- temp %>% group_by(home_team_api_id,annee)
test2<-cast(test,home_team_api_id~annee,value="buildUpPlaySpeedClass",fill=0)

# V1 : restitution données année avant prevision
temp <-filter(temp,annee=="2015")
temp <- temp [, c("home_team_api_id",
                  "buildUpPlaySpeedClass",
                  "buildUpPlayDribblingClass",
                  "buildUpPlayPassingClass","buildUpPlayPositioningClass",
                  "chanceCreationPassingClass",
                  "chanceCreationCrossingClass",
                  "chanceCreationShootingClass",
                  "chanceCreationPositioningClass",
                  "defencePressureClass",
                  "defenceAggressionClass",
                  "defenceTeamWidthClass",
                  "defenceDefenderLineClass")]

#jointure 
t<-Team_home_viz %>% left_join (temp)
Team_home_viz<-t

# nom de l'équipe
t<-Team_home_viz %>% left_join (Team[,c(2,4)],by= c("home_team_api_id"="team_api_id"))
Team_home_viz<-t

# sauvegarde de l'objet Team_home_viz

save(Team_home_viz,file="Team_home_viz.RData")

#test de visualisation
paris <- filter( Team_home_viz,team_long_name=="Paris Saint-Germain")

names(paris)

paris2 <-as.data.frame(t(paris[,c("nb_goal_scored_2008/2009","nb_goal_scored_2009/2010",
                  "nb_goal_scored_2010/2011","nb_goal_scored_2011/2012",
                  "nb_goal_scored_2012/2013","nb_goal_scored_2013/2014",
                  "nb_goal_scored_2014/2015")]))

paris3<-melt(paris[,c("nb_goal_scored_2008/2009","nb_goal_scored_2009/2010",
                      "nb_goal_scored_2010/2011","nb_goal_scored_2011/2012",
                      "nb_goal_scored_2012/2013","nb_goal_scored_2013/2014",
                      "nb_goal_scored_2014/2015")])


#ggplot(paris3)+ aes(x =variable, y=value) + geom_point() 

#ggplot(paris3)+ aes(x =variable, y=value) + geom_bar(stat="identity")

ggplot(paris3)+ aes(x =variable, y=value,group=1)+geom_point() + geom_line(size=1)


paris4<-melt(paris[,c("nb_goal_scored_2008/2009","nb_goal_scored_2009/2010",
                      "nb_goal_scored_2010/2011","nb_goal_scored_2011/2012",
                      "nb_goal_scored_2012/2013","nb_goal_scored_2013/2014",
                      "nb_goal_scored_2014/2015",
                      "nb_goal_conceded_2008/2009","nb_goal_conceded_2009/2010",
                      "nb_goal_conceded_2010/2011","nb_goal_conceded_2011/2012",
                      "nb_goal_conceded_2012/2013","nb_goal_conceded_2013/2014",
                      "nb_goal_conceded_2014/2015"
                      )])

paris4$season <- substr(as.character(paris4$variable),nchar(as.character(paris4$variable))-8,nchar(as.character(paris4$variable)) )

paris4$variable <- substr(as.character(paris4$variable),1,nchar(as.character(paris4$variable))-10 )

ggplot(paris4)+ aes(x =season, y=value, group=variable, colour=variable) +
geom_point(size=2) +
geom_line(size=1) + 
theme_classic() +
geom_text(aes(label=value), nudge_y = 4, size=3 ) +
labs(title="Nombre de buts marqués et encaissés par saison", x= "Saison", y ="Nombre de buts") +
theme(legend.position = "bottom") +
scale_color_manual(values=c("red", "green"))



test <- as.data.frame(t(paris [,51:62]))
