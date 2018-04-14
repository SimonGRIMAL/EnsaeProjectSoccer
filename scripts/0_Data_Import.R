
library(RSQLite)

##Connection a la base de donnees SQLite

filename <- "database.sqlite"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

##Importation des donnees

dbListTables(db)
Country <- dbReadTable(db,"Country")
League <- dbReadTable(db,"League")
Match <- dbReadTable(db,"Match")
Player <- dbReadTable(db,"Player")
Player_Attributes <- dbReadTable(db,"Player_Attributes")
Team <- dbReadTable(db,"Team")
Team_Attributes <- dbReadTable(db,"Team_Attributes")
sqlite_sequence <- dbReadTable(db,"sqlite_sequence")

dbDisconnect(db)

#############################################################################################################################################
#Construction de la Table MATCH_Shiny avec les infos necessaires pour l'affichage dans L'appli (saison 2015/2016 et hors championnat pologne)
Match_Shiny <- Match %>% filter(season=="2015/2016" & league_id != 15722) 
##creation colonnes pour nom complet des equipes PAS ABSOLUMENT NECESSAIRE POUR LE MOMENT
Match_Shiny$home_team_api_name <- Match_Shiny$home_team_api_id
Match_Shiny$away_team_api_name <- Match_Shiny$away_team_api_id
Match_Shiny$home_team_api_name <- Team$team_long_name[match(Match_Shiny$home_team_api_id  ,Team$team_api_id)]
Match_Shiny$away_team_api_name <- Team$team_long_name[match(Match_Shiny$away_team_api_id  ,Team$team_api_id)]
#Sauvegarde pour importation dans Shiny
save(Match_Shiny,file="SoccerApp/data/Match_Shiny.Rdata")
