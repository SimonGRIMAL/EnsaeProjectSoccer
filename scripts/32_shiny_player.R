load("data/Match_Shiny.Rdata")


test_m <- filter_match(Match_Shiny,"Paris Saint-Germain","FC Nantes")
test_m2<- test_m[,c(56:77)]

table(Match_Shiny$date)
#du 18/07/2015 au 25/05/2016


## creation de l'objet pour la visualisation des joueurs

table(Player_Attributes$date)
test<-as.data.frame(unique(Player_Attributes$date))


#filter la table team_attributes pour garder une ligne par joueur 
#et celle juste avant le début de la saison sur laquelle on prédit

Player_Attributes_f <-  Player_Attributes %>% filter(date <= "2015-07-18 00:00:00")%>% group_by(player_api_id)%>% filter(date == max(date))

Player_viz<-Player[,c(2,3,5,6,7)] %>%left_join(Player_Attributes_f[,c(3,5,7,8,9)], by=c("player_api_id"))

length(unique(Player_viz$player_api_id))

#doublons

test<- Player_Attributes_f%>% group_by(player_api_id) %>%count()%>%filter(n>1)
test2<-Player_Attributes_f%>% filter(player_api_id %in% c(37254,41308,110189,178196,193866))

Player_viz<-Player_viz[which(!(is.na(Player_viz$overall_rating))),]


save(Player_viz,file="data/Player_viz.RData")

#test fonction 

test_m3<- as.data.frame(t(test_m2))
colnames(test_m3)<-"player_api_id"
test_m4<-test_m3%>%inner_join(Player_viz,by=c("player_api_id"))

test_m5<-as.data.frame(t(test_m4))





t<-extract_attributes_player("Paris Saint-Germain","FC Nantes")
