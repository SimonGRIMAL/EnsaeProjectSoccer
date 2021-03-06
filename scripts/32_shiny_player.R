load("data/Match_Shiny.Rdata")


test_m <- filter_match(Match_Shiny,"Paris Saint-Germain","FC Nantes")
test_m2<- test_m[,c(56:77)]

table(Match_Shiny$date)
#du 18/07/2015 au 25/05/2016


## creation de l'objet pour la visualisation des joueurs

table(Player_Attributes$date)
test<-as.data.frame(unique(Player_Attributes$date))


#filter la table team_attributes pour garder une ligne par joueur 
#et celle juste avant le dC)but de la saison sur laquelle on prC)dit

Player_Attributes_f <-  Player_Attributes %>% filter(date <= "2015-07-18 00:00:00")%>% group_by(player_api_id)%>% filter(date == max(date))

Player_viz<-Player[,c(2,3,5,6,7)] %>%left_join(Player_Attributes_f[,c(3,5,7,8,9)], by=c("player_api_id"))

length(unique(Player_viz$player_api_id))

#doublons

test<- Player_Attributes_f%>% group_by(player_api_id) %>%count()%>%filter(n>1)
test2<-Player_Attributes_f%>% filter(player_api_id %in% c(37254,41308,110189,178196,193866))

Player_viz<-Player_viz[which(!(is.na(Player_viz$overall_rating))),]


save(Player_viz,file="data/Player_viz.RData")

#mise en forme

colnames(Player_viz)<-c("player_api_id","Name","Age","Height","Weight","Overall rating","Preferred foot","Attacking work rate","Defensive work rate")
library(lubridate)

Player_viz$Age=trunc(time_length(interval(as.Date(substr(Player_viz$Age,1,10)), as.Date(substr("2015-07-18 00:00:00",1,10))), "years"))

Player_viz$Height<-paste(floor(Player_viz$Height/100),"m",round(Player_viz$Height-floor(Player_viz$Height/100)*100),sep="")

Player_viz$Weight<-paste(round(Player_viz$Weight*0.453592),"kg",sep="")

Player_viz$Age<-as.integer(Player_viz$Age)


#Player_viz<-Player_viz[,c(1,2,3,4,5,7,6,8,9)]

Player_viz$`Preferred foot`<-NULL

save(Player_viz,file="data/Player_viz.RData")




